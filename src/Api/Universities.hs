{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities (UniversitiesAPI, universitiesServer) where

import Api.Universities.Courses (CoursesAPI, coursesServer)
import Api.Universities.Instructors (InstructorsAPI, instructorsServer)
import Api.Universities.Logo (LogoAPI, logoServer)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Universities (allUniversities, insertUniversity, removeUniversity, universityById)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth
import qualified Types.Auth.User as AU
import Types.University
import qualified Types.University.NewUniversity as NU
import Upload.Environment (S3Environment)

type GetAllUniversities =
  Summary "Get all universities"
    :> Get '[JSON] [University]

type GetUniversityById =
  Summary "Get university by ID"
    :> Capture' '[Required, Description "ID of the university"] "id" Int
    :> Get '[JSON] University

type DeleteUniversity =
  Summary "Delete university by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the unviersity"] "id" Int
    :> Verb 'DELETE 204 '[JSON] NoContent

type RegisterUniversity =
  JWTAuth
    :> Summary "Register a university"
    :> ReqBody '[JSON] NU.NewUniversity
    :> PostCreated '[JSON] University

type UniversitiesAPI =
  "universities"
    :> ( GetAllUniversities
          :<|> RegisterUniversity
          :<|> DeleteUniversity
          :<|> GetUniversityById
          :<|> CoursesAPI
          :<|> InstructorsAPI
          :<|> LogoAPI
       )

universitiesServer :: Pool Connection -> S3Environment -> Server UniversitiesAPI
universitiesServer conns s3env =
  getAllUniversities
    :<|> registerUniversity
    :<|> deleteUniversity
    :<|> getUniversityById
    :<|> coursesServer conns
    :<|> instructorsServer conns
    :<|> logoServer conns s3env
 where
  getAllUniversities :: Handler [University]
  getAllUniversities = liftIO $ allUniversities conns

  getUniversityById :: Int -> Handler University
  getUniversityById universityId = do
    mbUniversity <- liftIO $ universityById conns universityId

    case mbUniversity of
      Nothing -> throwError err404
      Just university -> return university

  registerUniversity :: AuthResult AU.AuthUser -> NU.NewUniversity -> Handler University
  registerUniversity (Authenticated authUser) newUniversity = do
    requireAdmin authUser
    result <-
      liftIO $
        try $
          insertUniversity conns newUniversity ::
        Handler (Either SqlError University)

    case result of
      Left _ -> throwError err400
      Right university -> return university
  registerUniversity _ _ = throwError err401

  deleteUniversity :: AuthResult AU.AuthUser -> Int -> Handler NoContent
  deleteUniversity (Authenticated authUser) universityId = do
    requireAdmin authUser

    liftIO $ removeUniversity conns universityId

    return NoContent
  deleteUniversity _ _ = throwError err401
