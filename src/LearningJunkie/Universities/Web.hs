{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web (API, server) where

import qualified LearningJunkie.Universities.Web.All as All
import qualified LearningJunkie.Universities.Web.Create as Create
import qualified LearningJunkie.Universities.Web.Specific as Specific
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "universities"
        :> ( All.API
                :<|> Create.API
                :<|> Specific.API
           )

server :: ServerT API AppM
server = All.handler :<|> Create.handler :<|> Specific.handler
