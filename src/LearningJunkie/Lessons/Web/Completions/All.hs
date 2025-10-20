{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Completions.All where

import Data.Int (Int32)
import LearningJunkie.LessonCompletions.Database (selectAllLessonCompletionsByLessonId, toLessonCompletionType)
import LearningJunkie.LessonCompletions.LessonCompletion (LessonCompletion)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "See all lesson completions by lesson ID"
        :> Get '[JSON] [LessonCompletion]

handler :: Int32 -> AppM [LessonCompletion]
handler lessonId = map toLessonCompletionType <$> selectAllLessonCompletionsByLessonId lessonId
