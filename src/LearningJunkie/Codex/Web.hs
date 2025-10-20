{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Codex.Web where

import qualified LearningJunkie.Codex.Web.Test as Test
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "codex" :> Test.API

server :: ServerT API AppM
server = Test.handler
