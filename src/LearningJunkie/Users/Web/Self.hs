{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self where

import qualified LearningJunkie.Users.Web.Self.Avatar as Avatar
import qualified LearningJunkie.Users.Web.Self.Completions as Completions
import qualified LearningJunkie.Users.Web.Self.LessonCompletions as LessonCompletions
import qualified LearningJunkie.Users.Web.Self.Progress as Progress
import qualified LearningJunkie.Users.Web.Self.Submissions as Submissions
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "self"
        :> ( Avatar.API
                :<|> Progress.API
                :<|> Completions.API
                :<|> LessonCompletions.API
                :<|> Submissions.API
           )

server :: ServerT API AppM
server =
    Avatar.server
        :<|> Progress.handler
        :<|> Completions.handler
        :<|> LessonCompletions.handler
        :<|> Submissions.handler
