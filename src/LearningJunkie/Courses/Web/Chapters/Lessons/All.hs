{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.Lessons.All where

import Data.Int (Int32)
import LearningJunkie.Lessons.Database (selectAllLessons, toLessonType)
import LearningJunkie.Lessons.Lesson (Lesson)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all lessons by course ID and chapter number"
        :> Get '[JSON] [Lesson]

handler :: Int32 -> Int32 -> AppM [Lesson]
handler courseId chapterNumber =
    map toLessonType
        <$> selectAllLessons courseId chapterNumber
