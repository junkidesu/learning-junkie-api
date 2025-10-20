{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Completions where

import Data.Int (Int32)
import qualified LearningJunkie.Lessons.Web.Completions.Add as Add
import qualified LearningJunkie.Lessons.Web.Completions.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "completions" :> (All.API :<|> Add.API)

server :: Int32 -> ServerT API AppM
server lessonId =
    All.handler lessonId
        :<|> Add.handler lessonId
