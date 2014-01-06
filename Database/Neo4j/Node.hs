{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j.Node
    ( Node(..)
    , createNode
    , deleteNode
    , getNode
    ) where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid
import Database.Neo4j.Core
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

data Node a = Node
    { nodeId :: Integer
    , nodeSelf :: B.ByteString
    , nodeProperties :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (Node a) where
    parseJSON (Object n) = do
        self <- n .: "self"
        props <- n .: "data"
        return Node
            { nodeId = getId self
            , nodeSelf =  self
            , nodeProperties = props }
        where
            getId :: B.ByteString -> Integer
            getId b = case BC.readInteger . snd $ BC.spanEnd (/= '/') b of
                Nothing -> throw ClientParseException
                Just (i, _) -> i
    parseJSON _ = mzero

getNode :: FromJSON a => Integer -> Neo4j (Either Neo4jError (Node a))
getNode node = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/node/", BC.pack $ show node] }
    liftIO $ sendRequest req' manager

createNode :: (ToJSON a, FromJSON a)
           => Maybe a
           -> Neo4j (Either Neo4jError (Node a))
createNode props = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = "db/data/node"
                   , method = "POST" }
    liftIO $ sendRequest (applyBody req' props) manager

applyBody :: ToJSON a => Request -> Maybe a -> Request
applyBody r p =
    case p of
        Nothing -> r
        Just p' -> r { requestBody = RequestBodyLBS $ encode p' }

deleteNode :: Integer -> Neo4j (Either Neo4jError ())
deleteNode node = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/node/", BC.pack $ show node]
                   , method = "DELETE" }
    liftIO $ sendRequest req' manager
