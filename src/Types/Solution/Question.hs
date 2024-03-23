{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Solution.Question (QuestionSolution (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Types.Exercise.Question as Q
import Types.Solution.ExerciseCheck
import Types.Solvable (Solvable (checkSolution))

newtype QuestionSolution = QuestionSolution
    { answer :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON QuestionSolution
instance ToJSON QuestionSolution
instance ToSchema QuestionSolution

instance Solvable Q.Question QuestionSolution where
    checkSolution :: Q.Question -> QuestionSolution -> ExerciseCheck
    checkSolution question solution =
        if Q.answer question == answer solution
            then ExerciseSuccess
            else ExerciseFailure
