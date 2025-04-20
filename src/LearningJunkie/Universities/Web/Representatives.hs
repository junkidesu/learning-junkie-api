{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Representatives where

import Data.Int (Int32)
import qualified LearningJunkie.Universities.Web.Representatives.Add as Add
import qualified LearningJunkie.Universities.Web.Representatives.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = All.API :<|> Add.API

server :: Int32 -> ServerT API AppM
server universityId =
    All.handler universityId
        :<|> Add.handler universityId
