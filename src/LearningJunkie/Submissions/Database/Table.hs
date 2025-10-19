{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Submissions.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (PgJSONB)
import LearningJunkie.Exercises.Database.Table (ExerciseT)
import LearningJunkie.Submissions.Submission.Content (SubmissionContent)
import LearningJunkie.Submissions.Submission.State (SubmissionState)
import LearningJunkie.Users.Database.Table (UserT)

data SubmissionT f = Submission
        { _submissionId :: C f Int32
        , _submissionUser :: PrimaryKey UserT f
        , _submissionExercise :: PrimaryKey ExerciseT f
        , _submissionContent :: C f (PgJSONB SubmissionContent)
        , _submissionState :: C f SubmissionState
        , _submissionGrade :: C f (Maybe Int32)
        , _submissionComment :: C f (Maybe Text)
        }
        deriving (Generic, Beamable)

type Submission = SubmissionT Identity
type SubmissionId = PrimaryKey SubmissionT Identity

deriving instance Show Submission
deriving instance Show SubmissionId

instance Table SubmissionT where
        data PrimaryKey SubmissionT f
                = SubmissionId
                        (C f Int32)
                deriving (Generic, Beamable)
        primaryKey = SubmissionId . _submissionId
