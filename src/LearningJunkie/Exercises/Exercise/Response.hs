{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module LearningJunkie.Exercises.Exercise.Response where

import Data.Aeson
import Data.Int (Int32)
import Data.OpenApi
import Data.Text
import GHC.Generics (Generic)
import LearningJunkie.Exercises.Exercise.Content.Response (ContentResponse)

data ExerciseResponse = ExerciseResponse
    { id :: Int32
    , title :: Text
    , description :: Text
    , maxGrade :: Int32
    , content :: ContentResponse
    }
    deriving (Show, Generic)

instance FromJSON ExerciseResponse
instance ToJSON ExerciseResponse
instance ToSchema ExerciseResponse
