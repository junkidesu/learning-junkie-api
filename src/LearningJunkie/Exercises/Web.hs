{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web (API, server) where

import Data.Int (Int32)
import qualified LearningJunkie.Exercises.Web.All as All
import qualified LearningJunkie.Exercises.Web.ById as ById
import qualified LearningJunkie.Exercises.Web.Delete as Delete
import qualified LearningJunkie.Exercises.Web.Submissions as Submissions
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "exercises"
        :> ( All.API
                :<|> ById.API
                :<|> Delete.API
                :<|> (Capture' '[Required, Description "ID of the exercise"] "id" Int32 :> Submissions.API)
           )

server :: ServerT API AppM
server =
    All.handler
        :<|> ById.handler
        :<|> Delete.handler
        :<|> Submissions.server
