{-# LANGUAGE DataKinds #-}

module Types.Auth.JWTAuth (JWTAuth) where

import Servant.Auth
import Types.Auth.User

type JWTAuth = Auth '[JWT] AuthUser
