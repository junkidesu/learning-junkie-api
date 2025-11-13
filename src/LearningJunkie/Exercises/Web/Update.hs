{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.Update where

import Data.Int (Int32)
import LearningJunkie.Exercises.Database (toExerciseResponseType, updateExercise)
import qualified LearningJunkie.Exercises.Exercise.Attributes as Attributes
import LearningJunkie.Exercises.Exercise.Response (ExerciseResponse)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Update exercise by exercise ID"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.Edit
        :> Put '[JSON] ExerciseResponse

handler :: Int32 -> AuthResult Auth.AuthUser -> Attributes.Edit -> AppM ExerciseResponse
handler exerciseId (Authenticated authUser) editExercise = Permissions.requirePermissionWithId
    authUser
    ( Permissions.Permission
        Permissions.SameInstructor
        Permissions.Delete
        Permissions.Exercise
    )
    (Just exerciseId)
    $ do
        toExerciseResponseType
            <$> updateExercise
                exerciseId
                editExercise
handler _ _ _ = throwError err401{errBody = "Authentication required"}
