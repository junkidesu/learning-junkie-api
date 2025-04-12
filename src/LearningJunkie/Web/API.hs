{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Web.API (API, server) where

import qualified LearningJunkie.Universities.Web as Universities
import LearningJunkie.Web.AppM (AppM)
import Servant
import qualified LearningJunkie.Users.Web as Users

type API = Universities.API :<|> Users.API

server :: ServerT API AppM
server = Universities.server :<|> Users.server
