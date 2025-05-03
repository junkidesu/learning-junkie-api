{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.ById where

import Data.Int (Int32)
import LearningJunkie.Lessons.Database (selectLessonById, toLessonType)
import LearningJunkie.Lessons.Lesson
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get lesson by ID" :> Get '[JSON] Lesson

handler :: Int32 -> AppM Lesson
handler lessonId = do
    mbLesson <- selectLessonById lessonId

    case mbLesson of
        Nothing -> throwError err404
        Just lesson -> return $ toLessonType lesson
