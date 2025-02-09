{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Universities.University where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data University = University
    { id :: !Int32
    , name :: !Text
    , abbreviation :: !(Maybe Text)
    , year :: !Int32
    , url :: !Text
    , logo :: !(Maybe Text)
    , joined :: !UTCTime
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON University
instance ToJSON University
instance ToSchema University
