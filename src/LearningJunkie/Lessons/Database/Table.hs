{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Lessons.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (PgJSONB)
import LearningJunkie.Chapters.Database.Table (ChapterT)
import LearningJunkie.Lessons.Lesson.Component (Component)

data LessonT f = Lesson
        { _lessonId :: C f Int32
        , _lessonLessonNumber :: C f Int32
        , _lessonChapter :: PrimaryKey ChapterT f
        , _lessonTitle :: C f Text
        , _lessonDescription :: C f Text
        , _lessonComponents :: C f (PgJSONB [Component])
        }
        deriving (Generic, Beamable)

instance Table LessonT where
        data PrimaryKey LessonT f
                = LessonId
                        (C f Int32)
                deriving (Generic, Beamable)
        primaryKey =
                LessonId
                        . _lessonId

type Lesson = LessonT Identity
type LessonId = PrimaryKey LessonT Identity

deriving instance Show Lesson
deriving instance Show LessonId
