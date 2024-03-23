{-# LANGUAGE DeriveGeneric #-}

module Types.Solution.ExerciseCheck (ExerciseCheck (..)) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import GHC.Generics

data ExerciseCheck = ExerciseSuccess | ExerciseFailure | ExercisePending
    deriving (Show, Eq, Generic)

instance ToJSON ExerciseCheck
instance ToSchema ExerciseCheck
