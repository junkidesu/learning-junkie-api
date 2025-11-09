{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.LessonCompletions where

import LearningJunkie.LessonCompletions.Database (selectAllLessonCompletionsByUserId, toLessonCompletionType)
import LearningJunkie.LessonCompletions.LessonCompletion (LessonCompletion)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Get all the lesson completions by the authenticated user"
        :> "lesson-completions"
        :> JWTAuth
        :> Get '[JSON] [LessonCompletion]

handler :: AuthResult Auth.AuthUser -> AppM [LessonCompletion]
handler (Authenticated authUser) =
    map toLessonCompletionType
        <$> selectAllLessonCompletionsByUserId (Auth.id authUser)
handler _ = throwError err401
