{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Courses where

import Data.Int
import qualified LearningJunkie.Universities.Web.Courses.Add as Add
import LearningJunkie.Web.AppM (AppM)
import Servant (ServerT)

type API = Add.API

server :: Int32 -> ServerT API AppM
server universityId = Add.handler universityId
