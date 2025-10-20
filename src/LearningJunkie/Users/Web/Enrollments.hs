{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Enrollments where

import Data.Int (Int32)
import qualified LearningJunkie.Users.Web.Enrollments.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "enrollments" :> All.API

server :: Int32 -> ServerT API AppM
server = All.handler
