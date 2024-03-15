{-# LANGUAGE DeriveGeneric #-}

module Types.University (University (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data University = University
    { id :: !Int
    , name :: !Text
    , abbreviation :: !(Maybe Text)
    , year :: !Int
    , joined :: !UTCTime
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON University
instance ToJSON University
instance FromRow University
instance ToRow University
instance ToSchema University
