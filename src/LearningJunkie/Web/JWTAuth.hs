{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Web.JWTAuth (JWTAuth, requireAdmin) where

import LearningJunkie.Users.Database.Role (Role (Admin))
import qualified LearningJunkie.Web.Auth.User as AU
import Servant (
    Handler,
    ServerError (errBody),
    err401,
    throwError,
 )
import Servant.Auth (Auth, JWT)
import Servant.Auth.OpenApi ()

type JWTAuth = Auth '[JWT] AU.AuthUser

requireAdmin :: AU.AuthUser -> Handler ()
requireAdmin authUser =
    case AU.role authUser of
        Admin -> pure ()
        _ -> throwError err401{errBody = "Unauthorized"}
