{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.All where

import LearningJunkie.Exercises.Database (selectAllExercises, toExerciseType)
import LearningJunkie.Exercises.Exercise (Exercise)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get all exercises" :> Get '[JSON] [Exercise]

handler :: AppM [Exercise]
handler = map toExerciseType <$> selectAllExercises
