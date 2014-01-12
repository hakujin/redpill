{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-}

module Database.Neo4j.Core
    ( Connection(..)
    , Neo4j(..)
    , Neo4jException(..)
    , Neo4jError(..)
    , authenticate
    , connect
    , parseId
    , runNeo4j
    , sendRequest
    , simpleNeo4j
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Typeable
import Data.Text.Read (decimal)
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

data Connection = Connection
    { connectionRequest :: Request
    , connectionManager :: Manager }

-- | All interaction with Neo4j is done through the 'Neo4j' monad.
-- Use 'runNeo4j' to run 'Neo4j' statements and evaluate their results.
newtype Neo4j a = Neo4j {
    runNeo :: ReaderT Connection (EitherT Neo4jError IO) a
    } deriving (Monad, MonadIO, MonadReader Connection, Functor, Applicative)

-- | Something terrible happened.
data Neo4jException =
                    -- | Occurs when aeson can't parse the returned JSON into
                    -- the data type you specified - e.g. the returned JSON
                    -- represents a Node User and you attempted to decode it
                    -- into a Relationship Giraffe. (what is wrong with you
                    -- aeson leave the giraffe alone)
                    ClientParseException
                    -- | Occurs when you feed 'connect' or 'simpleNeo4j' an
                    -- invalid URL.
                    | InvalidURLException
                    -- | Occurs when the Neo4j server is down/unreachable.
                    | ServerUnreachableException
                    deriving (Show, Typeable)

instance Exception Neo4jException

-- | Represents an error reply from the Neo4j server.
data Neo4jError =
    -- | @Neo4jError exception message@
    --
    -- [@exception@] The actual Neo4j exception - e.g. \"NodeNotFoundException\"
    --
    -- [@message@] The description of what exactly went wrong - e.g.
    -- \"Cannot find node with id [0] in database.\"
    Neo4jError T.Text T.Text
    deriving (Show, Eq)

instance FromJSON Neo4jError where
    parseJSON (Object e) = Neo4jError <$> e .: "exception" <*> e .: "message"
    parseJSON _ = mzero

neo4jUserEnv :: String
neo4jUserEnv = "NEO4J_LOGIN"

neo4jPasswordEnv :: String
neo4jPasswordEnv = "NEO4J_PASSWORD"

-- parses ID from Neo4j entity url
parseId :: T.Text -> Integer
parseId b =
    case decimal . snd $ T.breakOnEnd "/" b of
        Left _ -> throw ClientParseException
        Right (i, _) -> i

sendRequest :: FromJSON a => Request -> Manager -> IO (Either Neo4jError a)
sendRequest request manager = do
    res <- onException (httpLbs request manager)
                       (throwIO ServerUnreachableException)
    let code = statusCode $ responseStatus res
        body = responseBody res
    if code >= 200 && code < 300
        then maybe (throwIO ClientParseException) (return . Right) (decode body)
        else maybe (throwIO ClientParseException) (return . Left) (decode body)

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
                user' = BC.pack user
                pass' = BC.pack pass

-- | Interact with the Neo4j database specified by the given 'Connection'.
-- Each call of runNeo4j takes a network connection from the connection pool
-- and may block when all connections from the pool are in use.
--
-- @
-- connection <- connect \"http://localhost:7474\"
-- runNeo4j connection $ do
--     n1 <- getNode 1
--     n2 <- getNode 2
--     liftIO $ print (n1 :: Node User)
--     createRelationship n1 n2 \"FRIEND_OF\" Nothing
-- @
runNeo4j :: FromJSON a => Connection -> Neo4j a -> IO (Either Neo4jError a)
runNeo4j conn request = runEitherT $ runReaderT (runNeo request) conn

-- | Create a connection pool to the Neo4j server. 'connect' will automatically
-- use the NEO4J_LOGIN and NEO4J_PASSWORD environment variables for
-- authentication if defined. This function is relatively expensive and should
-- be called once then reused among multiple 'runNeo4j' instances.
--
-- @
-- connection <- connect \"http://localhost:7474\"
-- @
connect :: String -> IO Connection
connect url =
    case parseUrl url of
        Nothing -> throwIO InvalidURLException
        Just r -> do
            m <- newManager conduitManagerSettings
            r' <- authenticate r
            return Connection
                { connectionManager = m
                , connectionRequest =
                    r' { checkStatus = \_ _ _ -> Nothing
                       , requestHeaders = (hAccept, "application/json") :
                                          (hContentType, "application/json") :
                                          ("X-Stream", "true") :
                                          requestHeaders r' }}

-- | Run the specified 'Neo4j' request and return the parsed response body.
-- This function creates a new 'Connection' each time and is unsuitable for
-- production use.
--
-- @
-- node <- simpleNeo4j \"http://localhost:7474\" (getNode 1)
-- @
simpleNeo4j :: (FromJSON a) => String -> Neo4j a -> IO (Either Neo4jError a)
simpleNeo4j url f = do
    connection <- connect url
    runNeo4j connection f
