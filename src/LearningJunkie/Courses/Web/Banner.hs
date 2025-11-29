{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Banner where

import Data.Int (Int32)
import qualified LearningJunkie.Courses.Web.Banner.Remove as Remove
import qualified LearningJunkie.Courses.Web.Banner.Upload as Upload
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "banner" :> (Upload.API :<|> Remove.API)

server :: Int32 -> ServerT API AppM
server courseId = Upload.handler courseId :<|> Remove.handler courseId
