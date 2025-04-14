{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Web.API (API, server) where

import qualified LearningJunkie.Courses.Web as Courses
import qualified LearningJunkie.Universities.Web as Universities
import qualified LearningJunkie.Users.Web as Users
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Universities.API :<|> Users.API :<|> Courses.API

server :: ServerT API AppM
server = Universities.server :<|> Users.server :<|> Courses.server
