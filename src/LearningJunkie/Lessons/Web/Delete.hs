{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Delete where

import Data.Int (Int32)
import LearningJunkie.Lessons.Database (deleteLesson)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermissionWithId)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Delete a lesson by ID"
        :> JWTAuth
        :> Verb 'DELETE 204 '[JSON] NoContent

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM NoContent
handler lessonId (Authenticated authUser) = do
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Lesson
        )
        (Just lessonId)
        $ do
            deleteLesson lessonId
            return NoContent
handler _ _ = throwError err401
