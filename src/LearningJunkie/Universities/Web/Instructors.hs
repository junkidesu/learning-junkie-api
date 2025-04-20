{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Instructors where

import Data.Int (Int32)
import qualified LearningJunkie.Universities.Web.Instructors.Add as Add
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Add.API

server :: Int32 -> ServerT API AppM
server universityId = Add.handler universityId
