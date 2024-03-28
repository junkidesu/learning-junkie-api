{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities.Instructors (InstructorsAPI, instructorsServer) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations.Universities (universityById)
import Database.Operations.Universities.Instructors (allInstructors, insertInstructor)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth (JWTAuth, requireAdmin)
import qualified Types.Auth.User as AU
import Types.User (User)
import qualified Types.User.NewUser as NU

type GetInstructors =
  Summary "Get all instructors in a university"
    :> Get '[JSON] [User]

type AddInstructor =
  Summary "Add an instructor to a university"
    :> JWTAuth
    :> ReqBody '[JSON] NU.NewUser
    :> PostCreated '[JSON] User

type InstructorsAPI =
  Capture' '[Required, Description "ID of the university"] "id" Int
    :> "instructors"
    :> (GetInstructors :<|> AddInstructor)

instructorsServer :: Pool Connection -> Server InstructorsAPI
instructorsServer conns universityId = getInstructors :<|> addInstructor
 where
  ensureUniversityExists :: Handler ()
  ensureUniversityExists = ensureExists conns universityById universityId

  getInstructors :: Handler [User]
  getInstructors = do
    ensureUniversityExists
    liftIO $ allInstructors conns universityId

  addInstructor :: AuthResult AU.AuthUser -> NU.NewUser -> Handler User
  addInstructor (Authenticated authUser) newInstructor = do
    requireAdmin authUser
    ensureUniversityExists
    result <-
      liftIO $
        try $
          insertInstructor conns universityId newInstructor ::
        Handler (Either SqlError User)

    case result of
      Left _ -> throwError err400
      Right instructor -> return instructor
  addInstructor _ _ = throwError err401
