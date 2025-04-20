{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Delete where

import Data.Int (Int32)
import LearningJunkie.Courses.Database (deleteCourse)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Delete course by ID"
        :> JWTAuth
        :> Capture' '[Required, Description "ID of the course"] "id" Int32
        :> Verb 'DELETE 204 '[JSON] NoContent

handler :: AuthResult Auth.AuthUser -> Int32 -> AppM NoContent
handler (Authenticated authUser) courseId = Permissions.requirePermission
    authUser
    ( Permissions.Permission
        Permissions.SameUniversity
        Permissions.Delete
        Permissions.Course
    )
    $ do
        deleteCourse courseId
        return NoContent
handler _ _ = throwError err401
