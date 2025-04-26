{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.All where

import Data.Int (Int32)
import LearningJunkie.Chapters.Chapter (Chapter)
import LearningJunkie.Chapters.Database (selectAllCourseChapters, toChapterType)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all chapters of a course"
        :> Get '[JSON] [Chapter]

handler :: Int32 -> AppM [Chapter]
handler courseId = map toChapterType <$> selectAllCourseChapters courseId
