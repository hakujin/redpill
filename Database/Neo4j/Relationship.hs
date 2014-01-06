{-# LANGUAGE OverloadedStrings, TemplateHaskell  #-}

module Database.Neo4j.Relationship
    ( Relationship(..)
    , createRelationship
    , deleteRelationship
    , getRelationship
    ) where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Monoid
import Database.Neo4j.Core
import Database.Neo4j.Node
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

data Relationship a = Relationship
    { relationshipId :: Integer
    , relationshipSelf :: B.ByteString
    , relationshipType :: B.ByteString
    , relationshipStart :: B.ByteString
    , relationshipEnd :: B.ByteString
    , relationshipProperties :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (Relationship a) where
    parseJSON (Object r) = do
        relSelf <- r .: "self"
        relType <- r .: "type"
        relProps <- r .: "data"
        relStart <- r .: "start"
        relEnd <- r .: "end"
        return Relationship
            { relationshipId = getId relSelf
            , relationshipSelf = relSelf
            , relationshipType = relType
            , relationshipStart = relStart
            , relationshipEnd = relEnd
            , relationshipProperties = relProps }
        where
            getId :: B.ByteString -> Integer
            getId b = case BC.readInteger . snd $ BC.spanEnd (/= '/') b of
                Nothing -> throw ClientParseException
                Just (i, _) -> i
    parseJSON _ = mzero

data RelationshipRequest a = RelationshipRequest
    { _requestTo :: B.ByteString
    , _requestType :: T.Text
    , _requestData :: Maybe a } deriving (Show, Eq)

$(deriveJSON defaultOptions { omitNothingFields = True }
    { fieldLabelModifier = map toLower . drop 8 } ''RelationshipRequest)

getRelationship :: FromJSON a
                => Integer
                -> Neo4j (Either Neo4jError (Relationship a))
getRelationship rel = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/relationship/"
                                    , BC.pack $ show rel] }
    liftIO $ sendRequest req' manager

createRelationship :: (ToJSON c, FromJSON c)
                   => Node a -- ^ From
                   -> Node b -- ^ To
                   -> T.Text -- ^ Type
                   -> Maybe c -- ^ Properties
                   -> Neo4j (Either Neo4jError (Relationship c))
createRelationship from to relType props = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let body = encode RelationshipRequest
            { _requestTo = nodeSelf to
            , _requestType = relType
            , _requestData = props }
        req' = req { path = mconcat ["db/data/node/"
                                    , BC.pack . show $ nodeId from
                                    , "/relationships"]
                   , method = "POST"
                   , requestBody = RequestBodyLBS body }
    liftIO $ sendRequest req' manager

deleteRelationship :: Integer -> Neo4j (Either Neo4jError ())
deleteRelationship rel = Neo4j $ do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/relationship/"
                                    , BC.pack $ show rel]
                   , method = "DELETE" }
    liftIO $ sendRequest req' manager
