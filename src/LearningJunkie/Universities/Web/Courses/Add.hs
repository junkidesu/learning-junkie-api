{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Courses.Add where

import Data.Int
import LearningJunkie.Courses.Course (Course)
import qualified LearningJunkie.Courses.Course.Attributes as Attributes
import LearningJunkie.Courses.Database (insertCourse, toCourseType)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Add new course to a university"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] Course

handler :: Int32 -> AuthResult Auth.AuthUser -> Attributes.New -> AppM Course
handler universityId (Authenticated authUser) newCourse =
    Permissions.requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameUniversity
            Permissions.Create
            Permissions.Course
        )
        (Just universityId)
        $ do
            toCourseType <$> insertCourse newCourse universityId
handler _ _ _ = throwError err401
