{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Web.API (API, server) where

import qualified LearningJunkie.Certificates.Web as Certificates
import qualified LearningJunkie.Codex.Web as Codex
import qualified LearningJunkie.Courses.Web as Courses
import qualified LearningJunkie.Exercises.Web as Exercises
import qualified LearningJunkie.Lessons.Web as Lessons
import qualified LearningJunkie.Static.Web as Static
import qualified LearningJunkie.Submissions.Web as Submissions
import qualified LearningJunkie.Universities.Web as Universities
import qualified LearningJunkie.Users.Web as Users
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
  Universities.API
    :<|> Users.API
    :<|> Courses.API
    :<|> Lessons.API
    :<|> Exercises.API
    :<|> Submissions.API
    :<|> Codex.API
    :<|> Certificates.API
    :<|> Static.API

server :: ServerT API AppM
server =
  Universities.server
    :<|> Users.server
    :<|> Courses.server
    :<|> Lessons.server
    :<|> Exercises.server
    :<|> Submissions.server
    :<|> Codex.server
    :<|> Certificates.server
    :<|> Static.server
