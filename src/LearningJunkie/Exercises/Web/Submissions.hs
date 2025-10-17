{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.Submissions where

import Data.Int (Int32)
import qualified LearningJunkie.Exercises.Web.Submissions.Add as Add
import qualified LearningJunkie.Exercises.Web.Submissions.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "submissions" :> (All.API :<|> Add.API)

server :: Int32 -> ServerT API AppM
server exerciseId =
    All.handler exerciseId
        :<|> Add.handler exerciseId
