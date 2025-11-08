{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Courses.All where

import Data.Int (Int32)
import LearningJunkie.Courses.Course (Course)
import LearningJunkie.Courses.Database (selectCoursesByUniversityId, toCourseType)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all courses by university"
        :> Get '[JSON] [Course]

handler :: Int32 -> AppM [Course]
handler universityId =
    map toCourseType
        <$> selectCoursesByUniversityId universityId
