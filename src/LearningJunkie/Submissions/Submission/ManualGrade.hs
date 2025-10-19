{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Submissions.Submission.ManualGrade where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Submissions.Submission.State (SubmissionState)

data ManualGrade = ManualGrade
    { grade :: Int32
    , state :: SubmissionState
    , comment :: Text
    }
    deriving (Show, Generic)

instance FromJSON ManualGrade
instance ToJSON ManualGrade
instance ToSchema ManualGrade
