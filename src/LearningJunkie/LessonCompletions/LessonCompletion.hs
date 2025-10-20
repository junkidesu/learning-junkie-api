{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.LessonCompletions.LessonCompletion where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import LearningJunkie.Lessons.Lesson (Lesson)
import LearningJunkie.Users.User (User)

data LessonCompletion = LessonCompletion
    { id :: Int32
    , user :: User
    , lesson :: Lesson
    , time :: UTCTime
    }
    deriving (Generic)

instance FromJSON LessonCompletion
instance ToJSON LessonCompletion
instance ToSchema LessonCompletion
