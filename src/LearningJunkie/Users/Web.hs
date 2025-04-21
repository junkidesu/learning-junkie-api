{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web (API, server) where

import qualified LearningJunkie.Users.Web.All as All
import qualified LearningJunkie.Users.Web.ById as ById
import qualified LearningJunkie.Users.Web.Login as Login
import qualified LearningJunkie.Users.Web.Register as Register
import qualified LearningJunkie.Users.Web.Self as Self
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
  "users"
    :> ( All.API
          :<|> ById.API
          :<|> Register.API
          :<|> Login.API
          :<|> Self.API
       )

server :: ServerT API AppM
server =
  All.handler
    :<|> ById.handler
    :<|> Register.handler
    :<|> Login.handler
    :<|> Self.server
