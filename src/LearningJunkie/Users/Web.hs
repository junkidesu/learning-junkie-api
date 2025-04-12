{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module LearningJunkie.Users.Web (API, server) where

import LearningJunkie.Web.AppM (AppM)
import Servant 
import qualified LearningJunkie.Users.Web.All as All

type API = "users" :> (All.API)

server :: ServerT API AppM
server = All.handler
