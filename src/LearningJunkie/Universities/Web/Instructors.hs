{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Instructors where

import Data.Int (Int32)
import qualified LearningJunkie.Universities.Web.Instructors.Add as Add
import qualified LearningJunkie.Universities.Web.Instructors.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = All.API :<|> Add.API

server :: Int32 -> ServerT API AppM
server universityId =
    All.handler universityId
        :<|> Add.handler universityId
