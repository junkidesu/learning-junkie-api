{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.Delete where

import Data.Int (Int32)
import LearningJunkie.Exercises.Database (deleteExercise)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Delete exercise by ID"
        :> JWTAuth
        :> Verb 'DELETE 204 '[JSON] NoContent

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM NoContent
handler exerciseId (Authenticated authUser) =
    Permissions.requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Delete
            Permissions.Exercise
        )
        (Just exerciseId)
        $ do
            deleteExercise exerciseId
            return NoContent
handler _ _ = throwError err401
