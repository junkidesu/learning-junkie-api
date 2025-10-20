{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Completions.Add where

import Data.Int (Int32)
import LearningJunkie.LessonCompletions.Database (insertLessonCompletion, toLessonCompletionType)
import LearningJunkie.LessonCompletions.LessonCompletion (LessonCompletion)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Mark a lesson complete for authenticated user by ID"
        :> JWTAuth
        :> PostCreated '[JSON] LessonCompletion

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM LessonCompletion
handler lessonId (Authenticated authUser) = do
    toLessonCompletionType <$> insertLessonCompletion (Auth.id authUser) lessonId
handler _ _ = throwError err401
