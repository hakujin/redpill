{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j.Node
    ( Node(..)
    , createNode
    , deleteNode
    , getNode
    ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
-- import Data.Aeson.Encode.ByteString
import Data.Monoid
import Database.Neo4j.Core
import Network.HTTP.Conduit
import qualified Data.Text as T
-- import qualified Data.ByteString.Builder as B
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
getNode :: FromJSON a => Integer -> Neo4j (Either Neo4jError (Node a))
getNode node = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/node/", BC.pack $ show node] }
    liftIO $ sendRequest req' manager

-- | Create a 'Node' with optional properties.
createNode :: (ToJSON a, FromJSON a)
           => a
           -> Neo4j (Either Neo4jError (Node a))
createNode props = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = "db/data/node"
                   , method = "POST" }
    liftIO $ sendRequest (applyBody req' props) manager

-- TODO FIX THIS SHIT
applyBody :: (ToJSON a, FromJSON a) => Request -> a -> Request
applyBody r n = if j == emptyObject then r else r'
    where
        j = toJSON n
        encodeValue = encode --TLE.encodeUtf8 . TLB.toLazyText . encodeToTextBuilder
        r' = r { requestBody = RequestBodyLBS $ encodeValue j }

-- | Delete a 'Node' by Neo4j node ID.
deleteNode :: Integer -> Neo4j (Either Neo4jError ())
deleteNode node = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/node/", BC.pack $ show node]
                   , method = "DELETE" }
    liftIO $ sendRequest req' manager
