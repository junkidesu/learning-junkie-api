{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Solution.Quiz (QuizSolution (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Types.Exercise.Choice (Choice)
import qualified Types.Exercise.Quiz as Q
import Types.Solution.ExerciseCheck
import Types.Solvable (Solvable (checkSolution))

data QuizSolution = QuizSolution
    { answer :: !Choice
    }
    deriving (Generic)

instance ToJSON QuizSolution
instance FromJSON QuizSolution
instance ToSchema QuizSolution

instance Solvable Q.Quiz QuizSolution where
    checkSolution quiz solution =
        if Q.correct quiz == answer solution
            then ExerciseSuccess
            else ExerciseFailure
