{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Avatar where

import qualified LearningJunkie.Users.Web.Self.Avatar.Delete as Delete
import qualified LearningJunkie.Users.Web.Self.Avatar.Upload as Upload
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "avatar" :> (Upload.API :<|> Delete.API)

server :: ServerT API AppM
server = Upload.handler :<|> Delete.handler
