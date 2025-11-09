{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Chapters.Chapter where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Courses.Course (Course)

data Chapter = Chapter
    { number :: Int32
    , title :: Text
    , description :: Text
    , banner :: Maybe Text
    , course :: Course
    }
    deriving (Show, Generic)

instance FromJSON Chapter
instance ToJSON Chapter
instance ToSchema Chapter
