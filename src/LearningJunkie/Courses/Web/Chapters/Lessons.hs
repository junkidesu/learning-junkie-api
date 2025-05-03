{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.Lessons where

import Data.Int (Int32)
import qualified LearningJunkie.Courses.Web.Chapters.Lessons.Add as Add
import qualified LearningJunkie.Courses.Web.Chapters.Lessons.All as All
import qualified LearningJunkie.Courses.Web.Chapters.Lessons.Edit as Edit
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "lessons"
        :> ( All.API
                :<|> Add.API
                :<|> Edit.API
           )

server :: Int32 -> Int32 -> ServerT API AppM
server courseId chapterNumber =
    All.handler courseId chapterNumber
        :<|> Add.handler courseId chapterNumber
        :<|> Edit.handler courseId chapterNumber
