{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Submissions.Web where

import qualified LearningJunkie.Submissions.Web.Grade as Grade
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "submissions" :> Grade.API

server :: ServerT API AppM
server = Grade.handler
