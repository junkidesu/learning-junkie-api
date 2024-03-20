{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.NewQuestion (NewQuestion (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data NewQuestion = NewQuestion
    { grade :: !(Maybe Int)
    , question :: !Text
    , answer :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON NewQuestion
instance ToJSON NewQuestion
instance FromRow NewQuestion
instance ToSchema NewQuestion
