{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.EditQuestion (EditQuestion (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data EditQuestion = EditQuestion
    { question :: !Text
    , answer :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON EditQuestion
instance ToJSON EditQuestion
instance FromRow EditQuestion
instance ToSchema EditQuestion
