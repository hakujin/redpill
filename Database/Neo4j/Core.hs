{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-}

module Database.Neo4j.Core
    ( Connection(..)
    , Neo4j(..)
    , Neo4jException(..)
    , Neo4jError(..)
    , authenticate
    , connect
    , runNeo4j
    , sendRequest
    , simpleNeo4j
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Typeable
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

data Connection = Connection
    { connectionRequest :: Request
    , connectionManager :: Manager }

-- | All interaction with Neo4j is done through the Neo4j monad
newtype Neo4j a = Neo4j (ReaderT Connection IO a)
    deriving (Monad, MonadIO, Functor, Applicative)

data Neo4jException = ClientParseException
                    | InvalidURLException
                    | ServerUnreachableException
                    deriving (Show, Typeable)

instance Exception Neo4jException

data Neo4jError = Neo4jError
    T.Text  -- ^ exception
    T.Text -- ^ message
    deriving (Show, Eq)

instance FromJSON Neo4jError where
    parseJSON (Object e) = do
        exception <- e .: "exception"
        message <- e .: "message"
        return $ Neo4jError exception message
    parseJSON _ = mzero

neo4jUserEnv :: String
neo4jUserEnv = "NEO4J_LOGIN"

neo4jPasswordEnv :: String
neo4jPasswordEnv = "NEO4J_PASSWORD"

sendRequest :: FromJSON a => Request -> Manager -> IO (Either Neo4jError a)
sendRequest request manager = do
    res <- onException (httpLbs request manager)
                       (throw ServerUnreachableException)
    let status = responseStatus res
        code = statusCode status
        body = responseBody res
    if code >= 200 && code < 300
        then
            case decode body of
                Nothing -> throw ClientParseException
                Just body' -> return $ Right body'
        else
            case decode body of
                Nothing -> throw ClientParseException
                Just body' -> return $ Left body'

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

-- | Interact with a Neo4j database specified by the given Connection.
-- Each call of runNeo4j takes a network connection from the connection pool
-- and may block when all connections from the pool are in use.
runNeo4j :: FromJSON a => Connection -> Neo4j a -> IO a
runNeo4j conn (Neo4j request) = runReaderT request conn

-- | Create a connection pool to the Neo4j server.
connect :: String -> IO Connection
connect url =
    case parseUrl url of
        Nothing -> throw InvalidURLException
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

-- | Run the specified Neo4j request and return the parsed response body.
-- This function creates a new connection each time and is unsuitable for
-- production use.
simpleNeo4j :: FromJSON a => String -> Neo4j a -> IO a
simpleNeo4j url f = do
    connection <- connect url
    runNeo4j connection f
