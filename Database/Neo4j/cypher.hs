{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-}

module Database.Neo4j.Cypher
    ( Cypher
    , defaultConnectInfo
    , connect
    , cypher
    , runCypher
    , withConnection
    , Entity
    , CypherResult(..)
    , CypherException(..)
    , CypherVal(..)
    , CypherVals(..)
    , CypherCol(..)
    , CypherCols(..)
    , CypherMaybe(..)
    , CypherUnit(..)
    ) where

import Control.Applicative
import Control.Exception hiding (try, throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char (toLower)
import Data.List (elemIndices)
import Data.Monoid
import Data.Typeable
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | Information about your neo4j configuration needed to make requests over the REST api.
data ConnectInfo = ConnInfo
    { connectHost :: B.ByteString
    , connectPort :: Int
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 7 } ''ConnectInfo)

data Connection = Connection
    { connectionInfo :: ConnectInfo
    , connectionManager :: Manager }

-- | All interaction with Neo4j is done through the Cypher monad
-- Use 'cypher' to add a query to the monad.
newtype Cypher a = Cypher (ReaderT Connection IO a)
    deriving (Monad, MonadIO, Functor, Applicative)

-- | Raw result data returned by Neo4j. Only use this if you care about column headers.
data CypherResult a = CypherResult
    { resultColumns :: [T.Text]
    , resultData :: a
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 6 } ''CypherResult)

safeHead :: [a] -> Parser a
safeHead [a] = return a
safeHead _ = mzero

-- | A single result returned by Neo4j.
newtype CypherVal a = CypherVal a deriving (Show, Eq)

instance FromJSON a => FromJSON (CypherVal a) where
    parseJSON x = do
        CypherResult _ [[d]] <- parseJSON x
        return $ CypherVal d

-- | A single column returned by Neo4j.
newtype CypherCol a = CypherCol a deriving (Eq, Show)

instance FromJSON a => FromJSON (CypherCol a) where
    parseJSON x = do
        CypherResult _ [d] <- parseJSON x
        return $ CypherCol d

-- | Columns returned by Neo4j.
newtype CypherCols a = CypherCols a deriving (Show, Eq)

instance FromJSON a => FromJSON (CypherCols a) where
    parseJSON x = do
        CypherResult _ d <- parseJSON x
        return $ CypherCols d

-- | Values returned by Neo4j.
newtype CypherVals a = CypherVals [a] deriving (Show, Eq)

instance FromJSON a => FromJSON (CypherVals a) where
    parseJSON x = do
        CypherResult _ d <- parseJSON x
        fmap CypherVals $ mapM safeHead d

-- | Possibly a value returned by Neo4j
data CypherMaybe a = CypherJust a | CypherNothing deriving (Show, Eq)

instance FromJSON a => FromJSON (CypherMaybe a) where
    parseJSON x = do
        CypherResult _ ds <- parseJSON x
        case ds of
            [[d]] -> return $ CypherJust d
            _ -> return CypherNothing

-- | No value returned from Neo4j
data CypherUnit = CypherUnit deriving (Show)

instance FromJSON CypherUnit where parseJSON _ = return CypherUnit

data CypherRequest = CypherRequest
    { requestQuery :: T.Text
    , requestParams :: Value
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 7 } ''CypherRequest)

-- | A neo4j node or edge
data Entity a = Entity
    { entityId :: String -- ^ The Neo4j node or relationship id
    , entityProperties :: String
    , entityData :: a -- ^ The Haskell datatype stored in the Neo4j node or relationship
    } deriving (Show)

instance Eq (Entity a) where
    a == b = entityId a == entityId b

instance Ord (Entity a) where
    compare a b = compare (entityId a) (entityId b)

instance FromJSON a => FromJSON (Entity a) where
    parseJSON (Object v) =
        Entity <$> v .: "self" <*> v .: "properties" <*> v .: "data"
    parseJSON _ = mempty

instance ToJSON (Entity a) where
    toJSON a = toJSON (read x :: Int) where
        (_, _:x) = splitAt (last . elemIndices '/' $ entityId a) (entityId a)

-- | An error in handling a Cypher query, either in communicating with the server or parsing the result
data CypherException =
    CypherServerException Status ResponseHeaders BL.ByteString |
    CypherClientParseException B.ByteString deriving (Show, Typeable)

instance Exception CypherException

defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnInfo
    { connectHost = "localhost"
    , connectPort = 7474 }

cypher :: FromJSON a => T.Text -> Value -> Cypher a
cypher query params = Cypher $ do
    manager <- asks connectionManager
    info <- asks connectionInfo
    req <- liftIO $ parseUrl "http://google.com"
    let req' = req { host = connectHost info
            , port = connectPort info
            , path = "db/data/cypher"
            , requestBody = RequestBodyLBS . encode $ CypherRequest query params
            , requestHeaders = (hAccept, "application/json") :
                               (hContentType, "application/json") :
                               requestHeaders req
            , method = "POST" }
    res <- liftIO $ httpLbs req' manager
    case decode $ responseBody res of
        Nothing -> throw $ CypherServerException (responseStatus res)
                                                 (responseHeaders res)
                                                 (responseBody res)
        Just x -> return x

runCypher :: FromJSON a => Connection -> Cypher a -> IO a
runCypher conn (Cypher query) = runReaderT query conn

connect :: ConnectInfo -> IO Connection
connect ci = do
    m <- newManager conduitManagerSettings
    return  Connection
        { connectionInfo = ci
        , connectionManager = m }

withConnection :: FromJSON a => ConnectInfo -> Cypher a -> IO a
withConnection ci f = do
    connection <- connect ci
    runCypher connection f
