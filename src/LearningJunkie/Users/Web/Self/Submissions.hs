{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Submissions where

import qualified LearningJunkie.Users.Web.Self.Submissions.All as All
import qualified LearningJunkie.Users.Web.Self.Submissions.Successful as Successful
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "submissions"
        :> ( All.API
                :<|> Successful.API
           )

server :: ServerT API AppM
server =
    All.handler :<|> Successful.handler
