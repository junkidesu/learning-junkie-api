{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Solution.Essay (EssaySolution (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)
import qualified Types.Exercise.Essay as E
import Types.Solution.ExerciseCheck (ExerciseCheck (ExerciseSuccess))
import Types.Solvable (Solvable (checkSolution))

newtype EssaySolution = EssaySolution
    { answer :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON EssaySolution
instance ToJSON EssaySolution
instance ToSchema EssaySolution

instance Solvable E.Essay EssaySolution where
    checkSolution _ _ = ExerciseSuccess

instance FromRow EssaySolution
