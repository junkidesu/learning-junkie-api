{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.ById where

import Data.Int (Int32)
import LearningJunkie.Exercises.Database (selectExerciseById, toExerciseResponseType)
import LearningJunkie.Exercises.Exercise.Response (ExerciseResponse)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get exercise by ID"
        :> Capture' '[Required, Description "ID of the exercise"] "id" Int32
        :> Get '[JSON] ExerciseResponse

handler :: Int32 -> AppM ExerciseResponse
handler exerciseId = do
    mbExercise <- selectExerciseById exerciseId

    case mbExercise of
        Nothing -> throwError err404
        Just exercise -> return $ toExerciseResponseType exercise
