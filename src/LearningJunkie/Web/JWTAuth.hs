{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Web.JWTAuth (JWTAuth) where

import qualified LearningJunkie.Web.Auth.User as AU
import Servant.Auth (Auth, JWT)
import Servant.Auth.OpenApi ()

type JWTAuth = Auth '[JWT] AU.AuthUser
