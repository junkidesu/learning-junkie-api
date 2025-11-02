{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.CourseCompletions.Database.Table where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam
import LearningJunkie.Courses.Database.Table (CourseT)
import LearningJunkie.Users.Database.Table (UserT)

data CourseCompletionT f = CourseCompletion
        { _courseCompletionId :: C f UUID
        , _courseCompletionUser :: PrimaryKey UserT f
        , _courseCompletionCourse :: PrimaryKey CourseT f
        , _courseCompletionTime :: C f UTCTime
        }
        deriving (Generic, Beamable)

instance Table CourseCompletionT where
        data PrimaryKey CourseCompletionT f = CourseCompletionId (C f UUID)
                deriving (Generic, Beamable)
        primaryKey = CourseCompletionId . _courseCompletionId

type CourseCompletion = CourseCompletionT Identity
type CourseCompletionId = PrimaryKey CourseCompletionT Identity

deriving instance Show CourseCompletion
deriving instance Show CourseCompletionId
