{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Control.Monad.IO.Class
import Data.Aeson.TH
import Database.Neo4j

data User = User
    { name :: String
    , email :: String
    , created :: Integer } deriving Show

$(deriveJSON defaultOptions ''User)

main :: IO ()
main = do
    conn <- connect "http://localhost:7474"
    q <- readQuery "ck.cypher"
    xs <- runNeo4j conn $ do
        -- Right john <- createNode . Just $ User "John Smith" "jsmith@gmail.com" 42
        -- Right jane <- createNode . Just $ User "Jane Doe" "jdoe@hotmail.com" 37
        -- Right james <- createNode . Just $ User "James Jones" "jamesjones@aol.com" 29
        -- _ <- createRelationship jane john "LOVES" Empty
        -- _ <- createRelationship john jane "LIKES" Empty
        -- _ <- createRelationship james jane "LOVES" Empty
        -- Right rel <- createRelationship james john "DISLIKES" Empty
        -- liftIO . print $ nodeProperties john
        -- liftIO . print $ rel
        --
        --
        -- ns <- query "match (n) return n" Nothing
        -- bd <- createNode . Just $ User "Bob Dole" "bob@dole.com" 113109103910
        -- liftIO $ print bd
        -- nodeCk <- ck []
        -- ck <- query "match (u:Person {name:{NameGoesHere}}) return u.from" [("NameGoesHere", "Emil")]
        ck <- q [("NAME", "Emil")]
        liftIO $ print ck
        ee <- query "match (u:user {name:{NameGoesHere}}) return u, n" [("NameGoesHere", "Colin King")]
        liftIO $ print ee
        return ee
        -- liftIO $ print ck
        -- return ck
        -- xs <- query "match (n)-[r:CONTENT_OF]->(e:event) return n, r" Nothing
        -- Right ck <- getNode 16
        -- Right sc <- getNode 11
        -- Right rel <- createRelationship ck sc "FRIEND_OF" Nothing
        -- liftIO $ print (ck :: Node Object)
        -- liftIO $ print (sc :: Node User)
        -- xs <- query "match (u:user) return u" Nothing
    case xs of
        Left err -> print err
        Right xs' -> mapM_ (print . cRow) xs'
    where
        cRow :: [Value] -> Value
        cRow [u] = nodeProperties $ fromCypher u
        cRow _ = error "fucked up cRow"

-- convRow :: [Value] -> (Node Object, Relationship Object)
-- convRow [n, r] = (fromCypher n, fromCypher r)
-- convRow _ = error "fucked up convRow"
