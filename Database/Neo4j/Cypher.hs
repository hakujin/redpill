{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Database.Neo4j.Cypher
    ( fromCypher
    , fromNode
    , fromRelationship
    , readQuery
    , query
    , safeFromCypher
    ) where

import Control.Exception (throw)
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson (encode)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Database.Neo4j.Core
import Database.Neo4j.Node
import Database.Neo4j.Relationship
import Network.HTTP.Conduit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Cypher request data to be sent to Neo4j
data CypherRequest = CypherRequest
    { _requestQuery :: T.Text
    , _requestParams :: Value
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 8 } ''CypherRequest)

-- Cypher response data returned by Neo4j
data CypherResponse a = CypherResponse
    { _responseColumns :: [T.Text]
    , _responseData :: [[a]]
    } deriving (Show, Eq)

$(deriveJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 9 } ''CypherResponse)

-- | Reads a cypher statement from a file and returns a function that, when
-- evaluated inside a 'runNeo4j' block, will execute the given cypher statement.
-- This is the preferred method of running cypher statements as it keeps the
-- code separate, easy to refactor, and portable.
-- Throws 'IOException' on failure.
--
-- @
-- ...
-- main = do
--     connection <- 'connect' \"http://localhost:7474\"
--     -- get_users.cypher: MATCH (n:Users) RETURN n
--     getUsers <- 'readQuery' \"cypher/get_users.cypher\"
--     -- get_user_email.cypher: MATCH (n:Users {name:{NAME}}) RETURN n.email
--     getUserEmail <- 'readQuery' \"cypher/get_user_email.cypher\"
--     res <- 'runNeo4j' connection $ do
--         users <- getUsers []
--         johnEmail <- getUserEmail [(\"NAME\", \"John\")]
--         charlesEmail <- getUserEmail [(\"NAME\", \"Charles\")]
--         return (users, johnEmail, charlesEmail)
--     case res of
--         Left err -> print err
--         Right (u, j, c) -> do
--             mapM_ (print . convUser) u
--             -- assuming one User named \"John\" and one named \"Charles\"
--             putStrLn . 'fromCypher' . head $ head j
--             putStrLn . 'fromCypher' . head $ head c
--     where
--         convUser :: ['Value'] -> 'Node' User
--         convUser [u] = 'fromCypher' u
--         convUser _ = error \"didn't match the query return statement.\"
-- @
readQuery :: FilePath -> IO ([Pair] -> Neo4j [[Value]])
readQuery f = do
    q <- TIO.readFile f
    return $ query q

-- | Convert returned cypher data from 'Value' to some instance of FromJSON.
-- Throws 'ClientParseException' on invalid target.
fromCypher :: FromJSON a => Value -> a
fromCypher v = fromMaybe (throw ClientParseException) (parseMaybe parseJSON v)

-- | Convert returned cypher data from 'Value' to some instance of FromJSON.
-- Returns @Left \"error message\"@ on invalid target.
safeFromCypher :: FromJSON a => Value -> Either String a
safeFromCypher = parseEither parseJSON

-- | Convert returned cypher 'Node' properties directly into a Haskell data
-- type. This is synonymous with @'nodeProperties' . 'fromCypher'@
fromNode :: FromJSON a => Value -> a
fromNode = nodeProperties . fromCypher

-- | Convert returned cypher 'Relationship' properties directly into a Haskell
-- data type. This is synonymous with @'relationshipProperties' . 'fromCypher'@
fromRelationship :: FromJSON a => Value -> a
fromRelationship = relationshipProperties . fromCypher

-- | Execute a cypher query against the Neo4j database. Use 'fromCypher' or
-- 'safeFromCypher' to decode returned values into useful types.
query :: T.Text -> [Pair] -> Neo4j [[Value]]
query cypher params = do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let body = encode CypherRequest { _requestQuery = cypher
                                    , _requestParams = convertParams params }
        req' = req { path = "db/data/cypher"
                   , method = "POST"
                   , requestBody = RequestBodyLBS body }
    res <- liftIO $ sendRequest req' manager
    case res of
        Left err -> Neo4j . lift $ left err
        Right (CypherResponse _ d) -> return d
    where
        convertParams :: [Pair] -> Value
        convertParams [] = emptyObject
        convertParams p = object p
