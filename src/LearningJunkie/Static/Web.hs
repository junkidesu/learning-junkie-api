{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Static.Web where

import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "static" :> Raw

server :: ServerT API AppM
server = serveDirectoryWebApp "static/"
