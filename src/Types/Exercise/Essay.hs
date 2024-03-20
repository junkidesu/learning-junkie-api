{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.Essay (Essay (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data Essay = Essay
    { id :: !Int
    , grade :: !Int
    , task :: !Text
    , model :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON Essay
instance ToJSON Essay
instance FromRow Essay
instance ToSchema Essay
