{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Teaches where

import Data.Int (Int32)
import LearningJunkie.Courses.Course (Course)
import LearningJunkie.Courses.Database (selectCoursesByInstructorId, toCourseType)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all the courses that a user teaches if they are an instructor"
        :> "courses"
        :> Get '[JSON] [Course]

handler :: Int32 -> AppM [Course]
handler userId =
    map toCourseType
        <$> selectCoursesByInstructorId userId
