{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web (API, server) where

import qualified LearningJunkie.Exercises.Web.All as All
import qualified LearningJunkie.Exercises.Web.ById as ById
import qualified LearningJunkie.Exercises.Web.Delete as Delete
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "exercises" :> (All.API :<|> ById.API :<|> Delete.API)

server :: ServerT API AppM
server = All.handler :<|> ById.handler :<|> Delete.handler
