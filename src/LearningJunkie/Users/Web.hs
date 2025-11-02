{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web (API, server) where

import Data.Int (Int32)
import qualified LearningJunkie.Users.Web.All as All
import qualified LearningJunkie.Users.Web.ById as ById
import qualified LearningJunkie.Users.Web.Enrollments as Enrollments
import qualified LearningJunkie.Users.Web.Login as Login
import qualified LearningJunkie.Users.Web.Progress as Progress
import qualified LearningJunkie.Users.Web.Register as Register
import qualified LearningJunkie.Users.Web.Self as Self
import qualified LearningJunkie.Users.Web.Submissions as Submissions
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
  "users"
    :> ( All.API
          :<|> ById.API
          :<|> Register.API
          :<|> Login.API
          :<|> Self.API
          :<|> Capture' '[Required, Description "ID of the user"] "id" Int32
            :> ( Submissions.API
                  :<|> Enrollments.API
                  :<|> Progress.API
               )
       )

server :: ServerT API AppM
server =
  All.handler
    :<|> ById.handler
    :<|> Register.handler
    :<|> Login.handler
    :<|> Self.server
    :<|> ( \userId ->
            Submissions.server userId
              :<|> Enrollments.server userId
              :<|> Progress.handler userId
         )
