{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Exercises.All where

import Data.Int (Int32)
import LearningJunkie.Exercises.Database (selectExercisesByLessonId, toExerciseResponseType)
import LearningJunkie.Exercises.Exercise.Response (ExerciseResponse)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all exercises by lesson ID"
        :> Get '[JSON] [ExerciseResponse]

handler :: Int32 -> AppM [ExerciseResponse]
handler lessonId =
    map toExerciseResponseType
        <$> selectExercisesByLessonId lessonId
