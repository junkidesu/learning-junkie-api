{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web where

import qualified LearningJunkie.Courses.Web.All as All
import qualified LearningJunkie.Courses.Web.ById as ById
import qualified LearningJunkie.Courses.Web.Create as Create
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "courses"
        :> ( All.API
                :<|> Create.API
                :<|> ById.API
           )

server :: ServerT API AppM
server =
    All.handler
        :<|> Create.handler
        :<|> ById.handler
