{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Solution.Essay (EssaySolution (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Exercise.Essay as E
import Types.Solution.ExerciseCheck (ExerciseCheck (ExercisePending))
import Types.Solvable (Solvable (checkSolution))

newtype EssaySolution = EssaySolution
    { answer :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON EssaySolution
instance ToJSON EssaySolution
instance ToSchema EssaySolution

instance Solvable E.Essay EssaySolution where
    checkSolution _ _ = ExercisePending
