{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Database.Neo4j.Cypher
    ( fromCypher
    , fromNode
    , fromRelationship
    , query
    , safeFromCypher
    ) where

import Control.Exception (throw)
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Database.Neo4j.Core
import Database.Neo4j.Node
import Database.Neo4j.Relationship
import Network.HTTP.Conduit
import Data.Text (Text)

-- Cypher request data to be sent to Neo4j
data CypherRequest = CypherRequest
    { _requestQuery :: Text
    , _requestParams :: Value
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 8 } ''CypherRequest)

-- Cypher response data returned by Neo4j
data CypherResponse a = CypherResponse
    { _responseColumns :: [Text]
    , _responseData :: [[a]]
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 9 } ''CypherResponse)

-- | Convert returned cypher data from 'Value' to some instance of FromJSON.
-- Throws 'ClientParseException' on invalid target.
fromCypher :: FromJSON a => Value -> a
fromCypher v = fromMaybe (throw ClientParseException) (parseMaybe parseJSON v)

-- | Convert returned cypher data from 'Value' to some instance of FromJSON.
-- Returns @Left \"error message\"@ on invalid target.
safeFromCypher :: FromJSON a => Value -> Either String a
safeFromCypher = parseEither parseJSON

-- | Convert returned cypher 'Node' properties directly into a Haskell data
-- type. This is synonymous with @nodeProperties . fromCypher@
fromNode :: FromJSON a => Value -> a
fromNode = nodeProperties . fromCypher

-- | Convert returned cypher 'Relationship' properties directly into a Haskell
-- data type. This is synonymous with @relationshipProperties . fromCypher@
fromRelationship :: FromJSON a => Value -> a
fromRelationship = relationshipProperties . fromCypher

-- | Execute a cypher query against the Neo4j database. Use 'fromCypher' or
-- 'safeFromCypher' to decode returned values into useful types.
query :: Text -> Maybe [Pair] -> Neo4j (Either Neo4jError [[Value]])
query cypher params = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let body = encode CypherRequest { _requestQuery = cypher
                                    , _requestParams = convertParams params }
        req' = req { path = "db/data/cypher"
                   , method = "POST"
                   , requestBody = RequestBodyLBS body }
    res <- liftIO $ sendRequest req' manager
    return $ case res of
        Left err -> Left err
        Right (CypherResponse _ d) -> Right d
    where
        convertParams :: Maybe [Pair] -> Value
        convertParams = maybe emptyObject object
