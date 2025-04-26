{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Chapters.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import LearningJunkie.Courses.Database.Table (CourseT)

data ChapterT f = Chapter
        { _chapterChapterNumber :: C f Int32
        , _chapterCourse :: PrimaryKey CourseT f
        , _chapterTitle :: C f Text
        , _chapterDescription :: C f Text
        , _chapterBanner :: C f (Maybe Text)
        }
        deriving (Generic, Beamable)

instance Table ChapterT where
        data PrimaryKey ChapterT f
                = ChapterId
                        (PrimaryKey CourseT f)
                        (C f Int32)
                deriving (Generic, Beamable)
        primaryKey = ChapterId <$> _chapterCourse <*> _chapterChapterNumber

type Chapter = ChapterT Identity
type ChapterId = PrimaryKey ChapterT Identity
