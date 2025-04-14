{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.All where

import LearningJunkie.Courses.Course (Course)
import LearningJunkie.Courses.Database (selectAllCourses, toCourseType)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all courses"
        :> Get '[JSON] [Course]

handler :: AppM [Course]
handler = map toCourseType <$> selectAllCourses
