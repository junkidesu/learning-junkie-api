{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Exercises where

import Control.Lens.Internal.CTypes (Int32)
import qualified LearningJunkie.Lessons.Web.Exercises.Add as Add
import qualified LearningJunkie.Lessons.Web.Exercises.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "exercises" :> (All.API :<|> Add.API)

server :: Int32 -> ServerT API AppM
server lessonId =
    All.handler lessonId
        :<|> Add.handler lessonId
