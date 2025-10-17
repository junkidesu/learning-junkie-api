{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Submissions where

import Data.Int (Int32)
import qualified LearningJunkie.Users.Web.Submissions.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "submissions" :> All.API

server :: Int32 -> ServerT API AppM
server userId = All.handler userId
