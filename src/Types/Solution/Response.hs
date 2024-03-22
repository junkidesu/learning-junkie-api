{-# LANGUAGE DeriveGeneric #-}

module Types.Solution.Response (Response (..)) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)

data ExerciseCheck = ExerciseCheckSuccess | ExerciseCheckFailure | ExerciseCheckPending
    deriving (Show, Eq, Generic)

instance ToJSON ExerciseCheck
instance ToSchema ExerciseCheck

data Response = Response
    { result :: !ExerciseCheck
    , grade :: !(Maybe Int)
    }
    deriving (Show, Eq, Generic)

instance ToJSON Response
instance ToSchema Response
