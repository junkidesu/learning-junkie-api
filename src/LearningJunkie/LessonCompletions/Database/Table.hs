{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.LessonCompletions.Database.Table where

import Data.Int (Int32)
import Data.Time (UTCTime)
import Database.Beam
import LearningJunkie.Lessons.Database.Table (LessonT)
import LearningJunkie.Users.Database.Table (UserT)

data LessonCompletionT f = LessonCompletion
        { _lessonCompletionId :: C f Int32
        , _lessonCompletionUser :: PrimaryKey UserT f
        , _lessonCompletionLesson :: PrimaryKey LessonT f
        , _lessonCompletionTime :: C f UTCTime
        }
        deriving (Generic, Beamable)

instance Table LessonCompletionT where
        data PrimaryKey LessonCompletionT f = LessonCompletionId (C f Int32)
                deriving (Generic, Beamable)
        primaryKey =
                LessonCompletionId
                        . _lessonCompletionId

type LessonCompletion = LessonCompletionT Identity
type LessonCompletionId = PrimaryKey LessonCompletionT Identity

deriving instance Show LessonCompletion
deriving instance Show LessonCompletionId
