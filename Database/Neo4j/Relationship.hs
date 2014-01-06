{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j.Relationship
    ( Relationship(..)
    , createRelationship
    , deleteRelationship
    , getRelationship
    ) where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid
import Database.Neo4j.Core
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

data Relationship a = Relationship
    { relationshipId :: Integer
    , relationshipSelf :: B.ByteString
    , relationshipType :: B.ByteString
    , relationshipStart :: B.ByteString
    , relationshipEnd :: B.ByteString
    , relationshipData :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (Relationship a) where
    parseJSON (Object r) = do
        s <- r .: "self"
        t <- r .: "type"
        d <- r .: "data"
        start <- r .: "start"
        end <- r .: "end"
        return $ Relationship (getId s) s t d start end
        where
            getId :: B.ByteString -> Integer
            getId b = case BC.readInteger . snd $ BC.spanEnd (/= '/') b of
                Nothing -> throw ClientParseException
                Just (i, _) -> i
    parseJSON _ = mzero

getRelationship :: FromJSON a
                => Integer
                -> Neo4j (Either ServerError (Relationship a))
getRelationship rel = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/relationship/"
                                    , BC.pack $ show rel] }
    liftIO $ sendRequest req' manager

createRelationship :: (ToJSON a, FromJSON a)
                   => Maybe a
                   -> Neo4j (Either ServerError (Relationship a))
createRelationship props = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = "db/data/relationship"
                   , method = "POST" }
    liftIO $ sendRequest (applyBody req' props) manager
    where
        applyBody :: ToJSON a => Request -> Maybe a -> Request
        applyBody r p =
            case p of
                Nothing -> r
                Just p' -> r { requestBody = RequestBodyLBS $ encode p' }

deleteRelationship :: Integer -> Neo4j (Either ServerError ())
deleteRelationship rel = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/relationship/"
                                    ,  BC.pack $ show rel]
                   , method = "DELETE" }
    liftIO $ sendRequest req' manager
