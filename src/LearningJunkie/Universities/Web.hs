{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web (API, server) where

import Data.Int (Int32)
import qualified LearningJunkie.Universities.Web.All as All
import qualified LearningJunkie.Universities.Web.ById as ById
import qualified LearningJunkie.Universities.Web.Courses as Courses
import qualified LearningJunkie.Universities.Web.Create as Create
import qualified LearningJunkie.Universities.Web.Delete as Delete
import qualified LearningJunkie.Universities.Web.Instructors as Instructors
import qualified LearningJunkie.Universities.Web.Logo as Logo
import qualified LearningJunkie.Universities.Web.Representatives as Representatives
import qualified LearningJunkie.Universities.Web.Update as Update
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "universities"
        :> ( All.API
                :<|> Create.API
                :<|> ById.API
                :<|> Delete.API
                :<|> Update.API
                :<|> ( Capture' '[Required, Description "ID of the university"] "id" Int32
                        :> ( "instructors"
                                :> Instructors.API
                                :<|> "representatives"
                                    :> Representatives.API
                                :<|> "courses"
                                    :> Courses.API
                                :<|> Logo.API
                           )
                     )
           )

server :: ServerT API AppM
server =
    All.handler
        :<|> Create.handler
        :<|> ById.handler
        :<|> Delete.handler
        :<|> Update.handler
        :<|> ( \universityId ->
                Instructors.server universityId
                    :<|> Representatives.server universityId
                    :<|> Courses.server universityId
                    :<|> Logo.server universityId
             )
