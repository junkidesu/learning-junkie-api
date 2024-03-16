{-# LANGUAGE DeriveGeneric #-}

module Types.Course.NewCourse (NewCourse (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Course.Difficulty (Difficulty)

data NewCourse = NewCourse
    { title :: !Text
    , description :: !Text
    , difficulty :: !Difficulty
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON NewCourse
instance ToJSON NewCourse
instance ToSchema NewCourse
