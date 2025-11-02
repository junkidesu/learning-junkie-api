{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Logo where

import Data.Int (Int32)
import qualified LearningJunkie.Universities.Web.Logo.Upload as Upload
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "logo" :> Upload.API

server :: Int32 -> ServerT API AppM
server universityId = Upload.handler universityId
