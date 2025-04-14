{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Create where

import LearningJunkie.Courses.Course (Course)
import qualified LearningJunkie.Courses.Course.Attributes as Attributes
import LearningJunkie.Courses.Database (insertCourse, toCourseType)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Create a new course"
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] Course

handler :: Attributes.New -> AppM Course
handler newCourse = toCourseType <$> insertCourse newCourse
