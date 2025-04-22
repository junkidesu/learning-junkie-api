{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Enrollments.Enrollment where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import LearningJunkie.Courses.Course (Course)
import LearningJunkie.Users.User (User)

data Enrollment = Enrollment
    { user :: User
    , course :: Course
    , time :: UTCTime
    }
    deriving (Generic, Show)

instance FromJSON Enrollment
instance ToJSON Enrollment
instance ToSchema Enrollment
