{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Exercises.Add where

import Data.Int (Int32)
import LearningJunkie.Exercises.Database (insertExercise, toExerciseResponseType)
import qualified LearningJunkie.Exercises.Exercise.Attributes as Attributes
import LearningJunkie.Exercises.Exercise.Response (ExerciseResponse)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermissionWithId)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Add exercise to lesson by ID"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] ExerciseResponse

handler :: Int32 -> AuthResult Auth.AuthUser -> Attributes.New -> AppM ExerciseResponse
handler lessonId (Authenticated authUser) newExercise =
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Lesson
        )
        (Just lessonId)
        $ toExerciseResponseType <$> insertExercise lessonId newExercise
handler _ _ _ = throwError err401
