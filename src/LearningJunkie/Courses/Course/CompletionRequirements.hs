{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Courses.Course.CompletionRequirements where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)

data CompletionRequirements = CompletionRequirements
        { lessonPercentage :: Int32 -- value from 0 to 100
        , exercisePercentage :: Int32 -- value from 0 to 100
        , finalProject :: Bool -- whether mandatory or not
        }
        deriving (Generic, Show, Read, Eq)

instance FromJSON CompletionRequirements
instance ToJSON CompletionRequirements
instance ToSchema CompletionRequirements
