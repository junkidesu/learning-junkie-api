{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web (API, server) where

import Data.Int (Int32)
import qualified LearningJunkie.Exercises.Web.All as All
import qualified LearningJunkie.Exercises.Web.ById as ById
import qualified LearningJunkie.Exercises.Web.Delete as Delete
import qualified LearningJunkie.Exercises.Web.Submissions as Submissions
import qualified LearningJunkie.Exercises.Web.Update as Update
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "exercises"
        :> ( All.API
                :<|> ( Capture' '[Required, Description "ID of the exercise"] "id" Int32
                        :> ( ById.API
                                :<|> Update.API
                                :<|> Delete.API
                                :<|> Submissions.API
                           )
                     )
           )

server :: ServerT API AppM
server =
    All.handler
        :<|> \exerciseId ->
            ById.handler exerciseId
                :<|> Update.handler exerciseId
                :<|> Delete.handler exerciseId
                :<|> Submissions.server exerciseId
