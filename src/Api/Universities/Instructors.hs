{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities.Instructors (InstructorsAPI, instructorsServer) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Auth.JWTAuth (JWTAuth)
import Types.Instructor (Instructor)
import qualified Types.User.NewUser as NU

type GetInstructors =
  Summary "Get all instructors in a university"
    :> Get '[JSON] Instructor

type AddInstructor =
  Summary "Add an instructor to a university"
    :> JWTAuth
    :> ReqBody '[JSON] NU.NewUser
    :> PostCreated '[JSON] Instructor

type InstructorsAPI =
  Capture' '[Required, Description "ID of the university"] "id" Int
    :> "instructors"
    :> (GetInstructors :<|> AddInstructor)

instructorsServer :: Pool Connection -> Server InstructorsAPI
instructorsServer conns universityId = undefined
