{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Submissions.Submission.Content where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Exercises.Exercise.Quiz.Option (Option)

data SubmissionContent
    = TypeAnswer {typedAnswer :: Text}
    | TrueFalse {trueFalseAnswer :: Bool}
    | Essay {essayAnswer :: Text}
    | QuizAnswer {quizAnswer :: Option}
    deriving (Generic, Show)

instance FromJSON SubmissionContent
instance ToJSON SubmissionContent
instance ToSchema SubmissionContent
