{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters where

import Data.Int (Int32)
import qualified LearningJunkie.Courses.Web.Chapters.Add as Add
import qualified LearningJunkie.Courses.Web.Chapters.All as All
import qualified LearningJunkie.Courses.Web.Chapters.ByNumber as ByNumber
import qualified LearningJunkie.Courses.Web.Chapters.Delete as Delete
import qualified LearningJunkie.Courses.Web.Chapters.Lessons as Lessons
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "chapters"
        :> ( All.API
                :<|> Add.API
                :<|> ( Capture' '[Required, Description "Chapter number"] "chapterNumber" Int32
                        :> Lessons.API
                     )
                :<|> Delete.API
                :<|> ByNumber.API
           )

server :: Int32 -> ServerT API AppM
server courseId =
    All.handler courseId
        :<|> Add.handler courseId
        :<|> Lessons.server courseId
        :<|> Delete.handler courseId
        :<|> ByNumber.handler courseId
