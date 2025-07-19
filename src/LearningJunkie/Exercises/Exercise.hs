{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Exercises.Exercise where

import Data.Aeson
import Data.Int (Int32)
import Data.OpenApi
import Data.Text
import GHC.Generics (Generic)
import LearningJunkie.Exercises.Exercise.Content.Response (ContentResponse)

data Exercise
    = Exercise
    { id :: Int32
    , title :: Text
    , description :: Text
    , maxGrade :: Int32
    , content :: ContentResponse
    }
    deriving (Show, Generic)

instance FromJSON Exercise
instance ToJSON Exercise
instance ToSchema Exercise
