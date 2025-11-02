{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Courses.Course (Course (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Courses.Course.CompletionRequirements (CompletionRequirements)
import LearningJunkie.Courses.Course.Difficulty (Difficulty)
import LearningJunkie.Universities.University (University)
import LearningJunkie.Users.User (User)

data Course = Course
    { id :: Int32
    , title :: Text
    , description :: Text
    , difficulty :: Difficulty
    , banner :: Maybe Text
    , university :: University
    , instructor :: User
    , completionRequirements :: CompletionRequirements
    , totalLessonsNum :: Int32
    , totalExercisesNum :: Int32
    }
    deriving (Generic, Show)

instance FromJSON Course
instance ToJSON Course
instance ToSchema Course
