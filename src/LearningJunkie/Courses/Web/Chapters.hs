{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters where

import Data.Int (Int32)
import qualified LearningJunkie.Courses.Web.Chapters.Add as Add
import qualified LearningJunkie.Courses.Web.Chapters.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "chapters" :> (All.API :<|> Add.API)

server :: Int32 -> ServerT API AppM
server courseId = All.handler courseId :<|> Add.handler courseId
