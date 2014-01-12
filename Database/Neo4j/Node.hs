{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j.Node
    ( Node(..)
    , createNode
    , deleteNode
    , getNode
    ) where

import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Database.Neo4j.Core
import Network.HTTP.Conduit
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC

-- | A Neo4j node wrapper around a Haskell data type with ToJSON and
-- FromJSON instances.
data Node a = Node
    { nodeId :: Integer
    , nodeSelf :: T.Text
    , nodeProperties :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (Node a) where
    parseJSON (Object n) = do
        self <- n .: "self"
        props <- n .: "data"
        return Node
            { nodeId = parseId self
            , nodeSelf = self
            , nodeProperties = props }
    parseJSON _ = mzero

-- | Fetch a 'Node' by Neo4j node ID.
getNode :: FromJSON a => Integer -> Neo4j (Node a)
getNode node = do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/node/", BC.pack $ show node] }
    res <- liftIO $ sendRequest req' manager
    case res of
        Left err -> Neo4j . lift $ left err
        Right node' -> return node'

-- | Create a 'Node' with optional properties.
createNode :: (ToJSON a, FromJSON a) => a -> Neo4j (Node a)
createNode props = do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = "db/data/node"
                   , method = "POST" }
    res <- liftIO $ sendRequest (applyBody req' props) manager
    case res of
        Left err -> Neo4j . lift $ left err
        Right node' -> return node'

-- TODO: this toJSON encodes twice, but lets aeson deal with the new
-- 10.0.4.0 bytestring improvements. potential to optimize here.
applyBody :: (ToJSON a, FromJSON a) => Request -> a -> Request
applyBody r n =
    if toJSON n == emptyObject
        then r
        else r { requestBody = RequestBodyLBS $ encode n }

-- | Delete a 'Node' by Neo4j node ID.
deleteNode :: Integer -> Neo4j ()
deleteNode node = do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/node/", BC.pack $ show node]
                   , method = "DELETE" }
    res <- liftIO $ sendRequest req' manager
    case (res :: Either Neo4jError Value) of
        Left err -> Neo4j . lift $ left err
        Right _ -> return ()
