{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Update (API, handler) where

import Data.Int
import LearningJunkie.Courses.Course (Course)
import qualified LearningJunkie.Courses.Course.Attributes as Attributes
import LearningJunkie.Courses.Database (toCourseType, updateCourse)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Update course by ID"
        :> JWTAuth
        :> Capture' '[Required, Description "ID of the course"] "id" Int32
        :> ReqBody '[JSON] Attributes.Edit
        :> Put '[JSON] Course

handler :: AuthResult Auth.AuthUser -> Int32 -> Attributes.Edit -> AppM Course
handler (Authenticated authUser) courseId editCourse =
    Permissions.requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Course
        )
        (Just courseId)
        $ do
            toCourseType
                <$> updateCourse
                    courseId
                    editCourse
handler _ _ _ = throwError err401
