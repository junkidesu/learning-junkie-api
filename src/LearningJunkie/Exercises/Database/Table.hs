{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Exercises.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (PgJSONB)
import LearningJunkie.Exercises.Exercise.Content
import LearningJunkie.Lessons.Database.Table (LessonT)

data ExerciseT f = Exercise
    { _exerciseId :: C f Int32
    , _exerciseTitle :: C f Text
    , _exerciseDescription :: C f Text
    , _exerciseMaxGrade :: C f Int32
    , _exerciseContent :: C f (PgJSONB Content)
    , _exerciseLesson :: PrimaryKey LessonT f
    }
    deriving (Generic, Beamable)

instance Table ExerciseT where
    data PrimaryKey ExerciseT f = ExerciseId (C f Int32) deriving (Generic, Beamable)
    primaryKey = ExerciseId . _exerciseId

type Exercise = ExerciseT Identity
type ExerciseId = PrimaryKey ExerciseT Identity

deriving instance Show Exercise
deriving instance Show ExerciseId
