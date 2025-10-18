{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Lessons.Lesson where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import LearningJunkie.Lessons.Lesson.Component (Component)

data Lesson = Lesson
    { id :: Int32
    , number :: Int32
    , title :: Text
    , description :: Text
    , components :: [Component]
    }
    deriving (Show, Generic)

instance FromJSON Lesson
instance ToJSON Lesson
instance ToSchema Lesson
