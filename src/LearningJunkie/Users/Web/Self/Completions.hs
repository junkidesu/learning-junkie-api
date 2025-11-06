{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Completions where

import LearningJunkie.CourseCompletions.CourseCompletion (CourseCompletion)
import LearningJunkie.CourseCompletions.Database (selectCourseCompletionsByUserId, toCourseCompletionType)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Get all the course completions of the authenticated user"
        :> "completions"
        :> JWTAuth
        :> Get '[JSON] [CourseCompletion]

handler :: AuthResult Auth.AuthUser -> AppM [CourseCompletion]
handler (Authenticated authUser) =
    map
        toCourseCompletionType
        <$> selectCourseCompletionsByUserId (Auth.id authUser)
handler _ = throwError err401
