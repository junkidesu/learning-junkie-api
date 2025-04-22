{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Enrollments.All where

import Data.Int (Int32)
import LearningJunkie.Enrollments.Database (selectCourseEnrollmentsById, toEnrollmentType)
import LearningJunkie.Enrollments.Enrollment (Enrollment)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get all enrollments in a course" :> Get '[JSON] [Enrollment]

handler :: Int32 -> AppM [Enrollment]
handler courseId =
    map toEnrollmentType
        <$> selectCourseEnrollmentsById courseId
