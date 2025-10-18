{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Enrollments (API, server) where

import Data.Int (Int32)
import qualified LearningJunkie.Courses.Web.Enrollments.All as All
import qualified LearningJunkie.Courses.Web.Enrollments.Enroll as Enroll
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "enrollments" :> (All.API :<|> Enroll.API)

server :: Int32 -> ServerT API AppM
server courseId =
    All.handler courseId
        :<|> Enroll.handler courseId
