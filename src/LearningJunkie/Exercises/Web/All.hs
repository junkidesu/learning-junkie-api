{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.All where

import LearningJunkie.Exercises.Database (selectAllExercises, toExerciseResponseType)
import LearningJunkie.Exercises.Exercise.Response (ExerciseResponse)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get all exercises" :> Get '[JSON] [ExerciseResponse]

handler :: AppM [ExerciseResponse]
handler = map toExerciseResponseType <$> selectAllExercises
