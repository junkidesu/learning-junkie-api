{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Enrollments.Enroll where

import Data.Int (Int32)
import LearningJunkie.Enrollments.Database (insertEnrollment, toEnrollmentType)
import LearningJunkie.Enrollments.Enrollment (Enrollment)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Enroll in a course"
        :> JWTAuth
        :> Post '[JSON] Enrollment

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM Enrollment
handler courseId (Authenticated authUser) =
    toEnrollmentType
        <$> insertEnrollment
            (Auth.id authUser)
            courseId
handler _ _ = throwError err401
