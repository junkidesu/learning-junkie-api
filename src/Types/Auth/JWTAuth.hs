{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Auth.JWTAuth (JWTAuth, requireAdmin) where

import Servant
import Servant.Auth
import qualified Types.Auth.User as AU
import Types.User.Role

type JWTAuth = Auth '[JWT] AU.AuthUser

requireAdmin :: AU.AuthUser -> Handler ()
requireAdmin authUser =
    case AU.role authUser of
        Admin -> pure ()
        _ -> throwError err401{errBody = "Unauthorized"}
