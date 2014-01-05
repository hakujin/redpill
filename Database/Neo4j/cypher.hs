{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-}

module Database.Neo4j.Cypher
    ( Cypher
    , CypherException
    , Node(..)
    , Relationship(..)
    , connect
    , fromCypher
    , query
    , runCypher
    , safeFromCypher
    , simpleCypher
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson (decode, encode)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Monoid (mempty)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Environment (lookupEnv)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data Connection = Connection
    { connectionRequest :: Request
    , connectionManager :: Manager }

-- | All interaction with Neo4j is done through the Cypher monad
-- Use 'query' to add a query to the monad.
newtype Cypher a = Cypher (ReaderT Connection IO a)
    deriving (Monad, MonadIO, Functor, Applicative)

-- | Request data to be sent to Neo4j
data CypherRequest = CypherRequest
    { _requestQuery :: T.Text
    , _requestParams :: Value
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 8 } ''CypherRequest)

-- | Response data returned by Neo4j
data CypherResponse a = CypherResponse
    { _resultColumns :: [T.Text]
    , _resultData :: [[a]]
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 7 } ''CypherResponse)

data Node a = Node
    { nodeId :: T.Text
    , nodeData :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (Node a) where
    parseJSON (Object n) = do
        s <- n .: "self"
        d <- n .: "data"
        return $ Node (snd $ T.breakOnEnd "/" s) d
    parseJSON _ = mempty

data Relationship a = Relationship
    { relationshipId :: T.Text
    , relationshipType :: T.Text
    , relationshipData :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (Relationship a) where
    parseJSON (Object n) = do
        s <- n .: "self"
        t <- n .: "type"
        d <- n .: "data"
        return $ Relationship (snd $ T.breakOnEnd "/" s) t d
    parseJSON _ = mempty

data CypherException = ServerException Status ResponseHeaders BL.ByteString
                     | ClientParseException BL.ByteString
                     | ConnectionException String
                     deriving (Show, Typeable)

instance Exception CypherException

neo4jUserEnv :: String
neo4jUserEnv = "NEO4J_LOGIN"

neo4jPasswordEnv :: String
neo4jPasswordEnv = "NEO4J_PASSWORD"

unreachable :: String
unreachable = "The Neo4j server is unreachable."

authenticate :: Request -> IO Request
authenticate req = do
    envs <- runMaybeT $ do
        u <- MaybeT $ lookupEnv neo4jUserEnv
        p <- MaybeT $ lookupEnv neo4jPasswordEnv
        return (u, p)
    case envs of
        Nothing -> return req
        Just (user, pass) ->
            return $ applyBasicAuth user' pass' req
            where
                user' = encodeUtf8 $ T.pack user
                pass' = encodeUtf8 $ T.pack pass

safeFromCypher :: FromJSON a => Value -> Either String a
safeFromCypher = parseEither parseJSON

fromCypher :: FromJSON a => Value -> a
fromCypher v =
    case parseEither parseJSON v of
        Left err -> error err
        Right val -> val

query :: T.Text -> Maybe [Pair] -> Cypher [[Value]]
query cypher params = Cypher $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let body = encode . CypherRequest cypher $
            case params of
                Nothing -> emptyObject
                Just params' -> object $ map (uncurry (.=)) params'
        req' = req { path = "db/data/cypher"
                   , method = "POST"
                   , requestBody = RequestBodyLBS body }
    res <- liftIO . try $ httpLbs req' manager
    case res of
        Left (SomeException _) -> throw $ ConnectionException unreachable
        Right res' -> do
            let status = responseStatus res'
                body' = responseBody res'
            if status == status200 || status == status201
                then
                    case decode body' of
                        Nothing -> throw $ ClientParseException body'
                        Just (CypherResponse _ d) -> return d
                else throw $ ServerException status (responseHeaders res') body'

-- | Interact with a Neo4j database specified by the given Connection.
-- Each call of runCypher takes a network connection from the Connection pool
-- and may block when all connections from the pool are in use.
runCypher :: FromJSON a => Connection -> Cypher a -> IO a
runCypher conn (Cypher q) = runReaderT q conn

-- | Create a connection pool to the Neo4j server.
connect :: String -> IO Connection
connect url = do
    m <- newManager conduitManagerSettings
    case parseUrl url of
        Nothing -> throw $ ConnectionException unreachable
        Just r -> do
            r' <- authenticate r
            return Connection
                { connectionManager = m
                , connectionRequest =
                    r' { checkStatus = \_ _ _ -> Nothing
                       , requestHeaders = (hAccept, "application/json") :
                                          (hContentType, "application/json") :
                                          ("X-Stream", "true") :
                                          requestHeaders r' }}

-- | Run the specified cypher query and return the parsed response body.
-- This function creates a new connection each time; do not use in production.
simpleCypher :: String -> Cypher [[Value]] -> IO [[Value]]
simpleCypher url f = do
    connection <- connect url
    runCypher connection f
