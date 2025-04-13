{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web (API, server) where

import qualified LearningJunkie.Universities.Web.All as All
import qualified LearningJunkie.Universities.Web.ById as ById
import qualified LearningJunkie.Universities.Web.Create as Create
import qualified LearningJunkie.Universities.Web.Delete as Delete
import qualified LearningJunkie.Universities.Web.Update as Update
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "universities"
        :> ( All.API
                :<|> Create.API
                :<|> ById.API
                :<|> Delete.API
                :<|> Update.API
           )

server :: ServerT API AppM
server =
    All.handler
        :<|> Create.handler
        :<|> ById.handler
        :<|> Delete.handler
        :<|> Update.handler
