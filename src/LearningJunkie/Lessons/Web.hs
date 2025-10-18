{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web where

import qualified LearningJunkie.Lessons.Web.Delete as Delete
import qualified LearningJunkie.Lessons.Web.Edit as Edit

import Data.Int (Int32)
import qualified LearningJunkie.Lessons.Web.ById as ById
import qualified LearningJunkie.Lessons.Web.Exercises as Exercises
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "lessons"
        :> Capture' '[Required, Description "Lesson ID"] "id" Int32
        :> ( ById.API
                :<|> Delete.API
                :<|> Edit.API
                :<|> "exercises" :> Exercises.API
           )

server :: ServerT API AppM
server lessonId =
    ById.handler lessonId
        :<|> Delete.handler lessonId
        :<|> Edit.handler lessonId
        :<|> Exercises.server lessonId
