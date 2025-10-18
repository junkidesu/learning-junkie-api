{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.All (API, handler) where

import LearningJunkie.Users.Database (selectAllUsers, toUserType)
import LearningJunkie.Users.User (User)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API = Summary "Get all users" :> JWTAuth :> Get '[JSON] [User]

handler :: AuthResult Auth.AuthUser -> AppM [User]
handler (Authenticated authUser) = Permissions.requirePermission
    authUser
    ( Permissions.Permission
        Permissions.All
        Permissions.View
        Permissions.User
    )
    $ do
        map toUserType <$> selectAllUsers
handler _ = throwError err401{errBody = "Insufficient permissions"}
