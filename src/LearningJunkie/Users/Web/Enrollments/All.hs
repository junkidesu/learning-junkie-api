{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Enrollments.All where

import Data.Int (Int32)
import LearningJunkie.Enrollments.Database (selectEnrollmentsByUserId, toEnrollmentType)
import LearningJunkie.Enrollments.Enrollment (Enrollment)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all enrollments by a particular user"
        :> Get '[JSON] [Enrollment]

handler :: Int32 -> AppM [Enrollment]
handler userId = map toEnrollmentType <$> selectEnrollmentsByUserId userId
