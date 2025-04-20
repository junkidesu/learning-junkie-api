{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Delete (API, handler) where

import Data.Int (Int32)
import LearningJunkie.Universities.Database (deleteUniversity)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Delete university by ID"
        :> JWTAuth
        :> Capture' '[Required, Description "ID of the unviersity"] "id" Int32
        :> Verb 'DELETE 204 '[JSON] NoContent

handler :: AuthResult Auth.AuthUser -> Int32 -> AppM NoContent
handler (Authenticated authUser) universityId = Permissions.requirePermission
    authUser
    ( Permissions.Permission
        Permissions.All
        Permissions.Delete
        Permissions.University
    )
    $ do
        deleteUniversity universityId
        return NoContent
handler _ _ = throwError err401
