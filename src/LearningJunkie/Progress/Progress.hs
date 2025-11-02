{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Progress.Progress where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import LearningJunkie.Courses.Course (Course)
import LearningJunkie.Users.User (User)

data Progress = Progress
    { user :: User
    , course :: Course
    , lessonsCompleted :: Int32
    , exercisesCompleted :: Int32
    }
    deriving (Generic)

instance FromJSON Progress
instance ToJSON Progress
instance ToSchema Progress
