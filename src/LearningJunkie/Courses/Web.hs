{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web where

import qualified LearningJunkie.Courses.Web.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "courses" :> All.API

server :: ServerT API AppM
server = All.handler
