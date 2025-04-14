{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Courses.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
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
        }
        deriving (Generic, Beamable)

type Course = CourseT Identity
type CourseId = PrimaryKey CourseT Identity

instance Table CourseT where
        data PrimaryKey CourseT f = CourseId (C f Int32) deriving (Generic, Beamable)
        primaryKey = CourseId . _courseId
