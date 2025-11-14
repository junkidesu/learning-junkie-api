{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Progress.Progress where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import LearningJunkie.Enrollments.Enrollment (Enrollment)

data Progress = Progress
    { enrollment :: Enrollment
    , lessonsCompleted :: Int32
    , exercisesCompleted :: Int32
    }
    deriving (Generic)

instance FromJSON Progress
instance ToJSON Progress
instance ToSchema Progress
