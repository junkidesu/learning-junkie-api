{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Banner.Remove where

import Data.Int (Int32)
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
    Summary "Delete course banner"
        :> JWTAuth
        :> Verb 'DELETE 200 '[JSON] Course

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM Course
handler courseId (Authenticated authUser) = Permissions.requirePermissionWithId
    authUser
    (Permissions.Permission Permissions.SameInstructor Permissions.Update Permissions.Course)
    (Just courseId)
    $ do
        toCourseType
            <$> updateCourse
                courseId
                Attributes.emptyEditCourse{Attributes.banner = Just Nothing}
handler _ _ = throwError err401
