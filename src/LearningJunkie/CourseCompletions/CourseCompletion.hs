{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.CourseCompletions.CourseCompletion where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import LearningJunkie.Courses.Course (Course)
import LearningJunkie.Users.User (User)

data CourseCompletion = CourseCompletion
    { id :: !UUID
    , user :: !User
    , course :: !Course
    , time :: !UTCTime
    }
    deriving (Generic)

instance FromJSON CourseCompletion
instance ToJSON CourseCompletion
instance ToSchema CourseCompletion
