{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Submissions.Submission.Content where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data SubmissionContent
    = TypeAnswer {typedAnswer :: Text}
    | TrueFalse {trueFalseAnswer :: Text}
    | Essay {essayAnswer :: Text}
    | QuizAnswer {quizAnswer :: Text}
    deriving (Generic, Show, Read)

instance FromJSON SubmissionContent
instance ToJSON SubmissionContent
instance ToSchema SubmissionContent
