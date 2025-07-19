{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Exercises.All where

import Data.Int (Int32)
import LearningJunkie.Exercises.Database (selectExercisesByLessonId, toExerciseType)
import LearningJunkie.Exercises.Exercise (Exercise)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all exercises by lesson ID"
        :> Get '[JSON] [Exercise]

handler :: Int32 -> AppM [Exercise]
handler lessonId =
    map toExerciseType
        <$> selectExercisesByLessonId lessonId
