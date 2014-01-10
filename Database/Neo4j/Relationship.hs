{-# LANGUAGE OverloadedStrings, TemplateHaskell  #-}

module Database.Neo4j.Relationship
    ( Relationship(..)
    , createRelationship
    , deleteRelationship
    , getRelationship
    ) where

import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Monoid
import Database.Neo4j.Core
import Database.Neo4j.Node
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

-- | A Neo4j relationship wrapper around a Haskell data type with ToJSON and
-- FromJSON instances.
data Relationship a = Relationship
    { relationshipId :: Integer
    , relationshipSelf :: T.Text
    , relationshipType :: T.Text
    , relationshipStart :: T.Text
    , relationshipEnd :: T.Text
    , relationshipProperties :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (Relationship a) where
    parseJSON (Object r) = do
        relSelf <- r .: "self"
        relType <- r .: "type"
        relProps <- r .: "data"
        relStart <- r .: "start"
        relEnd <- r .: "end"
        return Relationship
            { relationshipId = parseId relSelf
            , relationshipSelf = relSelf
            , relationshipType = relType
            , relationshipStart = relStart
            , relationshipEnd = relEnd
            , relationshipProperties = relProps }
    parseJSON _ = mzero

data RelationshipRequest a = RelationshipRequest
    { _requestTo :: T.Text
    , _requestType :: T.Text
    , _requestData :: Maybe a } deriving (Show, Eq)

$(deriveJSON defaultOptions { omitNothingFields = True }
    { fieldLabelModifier = map toLower . drop 8 } ''RelationshipRequest)

-- | Fetch a 'Relationship' by Neo4j relationship ID.
getRelationship :: FromJSON a => Integer -> Neo4j (Relationship a)
getRelationship rel = do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/relationship/"
                                    , BC.pack $ show rel] }
    res <- liftIO $ sendRequest req' manager
    case res of
        Left err -> Neo4j . lift $ left err
        Right rel' -> return rel'

-- | Create a 'Relationship' with optional properties.
createRelationship :: (ToJSON c, FromJSON c)
                   => Node a -- ^ From
                   -> Node b -- ^ To
                   -> T.Text -- ^ Type
                   -> Maybe c -- ^ Properties
                   -> Neo4j (Relationship c)
createRelationship from to relType props = do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let body = encode RelationshipRequest { _requestTo = nodeSelf to
                                          , _requestType = relType
                                          , _requestData = props }
        req' = req { path = mconcat ["db/data/node/"
                                    , BC.pack . show $ nodeId from
                                    , "/relationships"]
                   , method = "POST"
                   , requestBody = RequestBodyLBS body }
    res <- liftIO $ sendRequest req' manager
    case res of
        Left err -> Neo4j . lift $ left err
        Right rel' -> return rel'

-- | Delete a 'Relationship' by Neo4j relationship ID.
deleteRelationship :: Integer -> Neo4j ()
deleteRelationship rel = do
    manager <- asks connectionManager
    req <- asks connectionRequest
    let req' = req { path = mconcat ["db/data/relationship/"
                                    , BC.pack $ show rel]
                   , method = "DELETE" }
    res <- liftIO $ sendRequest req' manager
    case (res :: Either Neo4jError Value) of
        Left err -> Neo4j . lift $ left err
        Right _ -> return ()
