{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.ByNumber where

import Data.Int (Int32)
import LearningJunkie.Chapters.Chapter (Chapter)
import LearningJunkie.Chapters.Database (selectChapterByCourseIdAndNumber, toChapterType)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get specific chapter by course ID and number"
        :> Capture' '[Required, Description "Number of the chapter"] "chapterNumber" Int32
        :> Get '[JSON] Chapter

handler :: Int32 -> Int32 -> AppM Chapter
handler courseId chapterNumber = do
    mbChapter <-
        selectChapterByCourseIdAndNumber
            courseId
            chapterNumber

    case mbChapter of
        Nothing -> throwError err404
        Just chapter -> pure $ toChapterType chapter
