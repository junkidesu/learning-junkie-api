{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.ById where

import Data.Int (Int32)
import LearningJunkie.Courses.Course (Course)
import LearningJunkie.Courses.Database (selectCourseById, toCourseType)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get course by ID"
        :> Capture' '[Required, Description "The ID of the course"] "id" Int32
        :> Get '[JSON] Course

handler :: Int32 -> AppM Course
handler courseId = do
    mbCourse <- selectCourseById courseId

    case mbCourse of
        Nothing -> throwError err404
        Just course -> return $ toCourseType course
