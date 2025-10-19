{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Submissions.Submission where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Exercises.Exercise (Exercise)
import LearningJunkie.Submissions.Submission.Content
import LearningJunkie.Submissions.Submission.State (SubmissionState)
import LearningJunkie.Users.User (User)

data Submission = Submission
    { id :: Int32
    , user :: User
    , exercise :: Exercise
    , content :: SubmissionContent
    , state :: SubmissionState
    , grade :: Maybe Int32
    , comment :: Maybe Text
    }
    deriving (Generic)

instance FromJSON Submission
instance ToJSON Submission
instance ToSchema Submission
