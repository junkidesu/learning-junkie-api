{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web where

import Data.Int (Int32)
import qualified LearningJunkie.Courses.Web.All as All
import qualified LearningJunkie.Courses.Web.ById as ById
import qualified LearningJunkie.Courses.Web.Chapters as Chapters
import qualified LearningJunkie.Courses.Web.Delete as Delete
import qualified LearningJunkie.Courses.Web.Enrollments as Enrollments
import qualified LearningJunkie.Courses.Web.Update as Update
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "courses"
        :> ( All.API
                :<|> ById.API
                :<|> Delete.API
                :<|> Update.API
                :<|> Capture' '[Required, Description "ID of the course"] "id" Int32
                    :> ( Enrollments.API
                            :<|> Chapters.API
                       )
           )

server :: ServerT API AppM
server =
    All.handler
        :<|> ById.handler
        :<|> Delete.handler
        :<|> Update.handler
        :<|> \courseId ->
            Enrollments.server courseId
                :<|> Chapters.server courseId
