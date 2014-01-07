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
--     Right john <- 'createNode' . Just $ User \"John Smith\" 42
--     Right jane <- 'createNode' . Just $ User \"Jane Doe\" 37
--     Right james <- 'createNode' . Just $ User \"James Jones\" 29
--     'createRelationship' jane john \"LOVES\" Nothing
--     'createRelationship' john jane \"LIKES\" Nothing
--     'createRelationship' james jane \"LOVES\" Nothing
--     Right rel <- 'createRelationship' james john \"DISLIKES\" $
--         Just [(\"amount\", \"a lot!\")]
--     liftIO . print $ 'nodeProperties' john
--     liftIO . print $ (rel :: 'Relationship' 'Value')
-- @
--
-- Send cypher queries to the server:
--
-- @
-- ...
-- -- assuming existing User nodes
-- 'runNeo4j' conn $ do
--     res <- 'query' \"match (n:User) return n, n.name, n.age\" Nothing
--     case res of
--         Left err -> liftIO $ print err
--         Right res' -> mapM_ (liftIO . print) (map convRow res')
--     where
--         convRow :: ['Value'] -> (User, String, Int)
--         convRow [u, n, a] = ('fromNode' u, 'fromCypher' n, 'fromCypher' a)
--         convRow _ = error \"didn't match query return values\"
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
    , runNeo4j
    , safeFromCypher
    , simpleNeo4j
    ) where

import Database.Neo4j.Core
import Database.Neo4j.Cypher
import Database.Neo4j.Node
import Database.Neo4j.Relationship
import Data.Aeson.Types
