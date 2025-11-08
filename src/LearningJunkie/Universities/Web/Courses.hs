{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Courses where

import Data.Int
import qualified LearningJunkie.Universities.Web.Courses.Add as Add
import qualified LearningJunkie.Universities.Web.Courses.All as All
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Add.API
        :<|> All.API

server :: Int32 -> ServerT API AppM
server universityId =
    Add.handler universityId
        :<|> All.handler universityId
