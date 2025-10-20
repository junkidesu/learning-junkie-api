{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Courses.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (PgJSONB)
import LearningJunkie.Courses.Course.CompletionRequirements (CompletionRequirements)
import LearningJunkie.Courses.Course.Difficulty (Difficulty)
import LearningJunkie.Universities.Database.Table (UniversityT)
import LearningJunkie.Users.Database.Table (UserT)

data CourseT f = Course
        { _courseId :: C f Int32
        , _courseTitle :: C f Text
        , _courseDescription :: C f Text
        , _courseDifficulty :: C f Difficulty
        , _courseBanner :: C f (Maybe Text)
        , _courseUniversity :: PrimaryKey UniversityT f
        , _courseInstructor :: PrimaryKey UserT f
        , _courseCompletionRequirements :: C f (PgJSONB CompletionRequirements)
        }
        deriving (Generic, Beamable)

type Course = CourseT Identity
type CourseId = PrimaryKey CourseT Identity

deriving instance Show Course
deriving instance Show CourseId

instance Table CourseT where
        data PrimaryKey CourseT f = CourseId (C f Int32) deriving (Generic, Beamable)
        primaryKey = CourseId . _courseId
