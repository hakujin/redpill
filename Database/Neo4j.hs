module Database.Neo4j (
-- * How To Use This Library
-- |
-- Connect to a Neo4j Server:
--
-- @
-- conn <- 'connect' \"http://localhost:7474\"
-- @
--
-- Send basic commands to the server:
--
-- @
-- import Data.Aeson.TH
-- data User = User { name :: String, age :: Int } deriving Show
-- $(deriveJSON defaultOptions ''User)
-- ...
-- 'runNeo4j' conn $ do
--     john <- 'createNode' $ User \"John Smith\" 42
--     jane <- 'createNode' $ User \"Jane Doe\" 37
--     james <- 'createNode' $ User \"James Jones\" 29
--     'createRelationship' jane \"LOVES\" john Empty
--     'createRelationship' john \"LIKES\" jane Empty
--     'createRelationship' james \"LOVES\" jane Empty
--     rel <- 'createRelationship' james \"ENVIOUS_OF\" john $
--         [(\"amount\", \"very\")]
--     liftIO . print $ 'nodeProperties' john
--     liftIO . print $ (rel :: 'Relationship' 'Value')
-- @
--
-- Send cypher queries to the server:
--
-- @
-- ...
-- -- assuming existing User nodes
-- res <- runNeo4j' conn $
--     'query' \"MATCH (n:User) RETURN n, n.name, n.age\" []
-- case res of
--     Left err -> print err
--     Right res' -> mapM_ (print . convRow) res'
-- where
--     convRow :: ['Value'] -> (User, String, Int)
--     convRow [u, n, a] = ('fromNode' u, 'fromCypher' n, 'fromCypher' a)
--     convRow _ = error \"didn't match query return statement\"
-- @

    Connection
    , Neo4j
    , Neo4jException(..)
    , Neo4jError(..)
    , Node(..)
    , Relationship(..)
    , Value
    , connect
    , createNode
    , createRelationship
    , deleteNode
    , deleteRelationship
    , fromCypher
    , fromNode
    , fromRelationship
    , getNode
    , getRelationship
    , query
    , readQuery
    , runNeo4j
    , safeFromCypher
    , simpleNeo4j
    ) where

import Database.Neo4j.Core
import Database.Neo4j.Cypher
import Database.Neo4j.Node
import Database.Neo4j.Relationship
import Data.Aeson.Types
