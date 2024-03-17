{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities (UniversitiesAPI, universitiesServer) where

import Api.Universities.Courses (CoursesAPI, coursesServer)
import Api.Universities.Instructors (InstructorsAPI, instructorsServer)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Universities (allUniversities, insertUniversity)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth
import qualified Types.Auth.User as AU
import Types.University
import qualified Types.University.NewUniversity as NU

type GetAllUniversities =
  Summary "Get all universities"
    :> Get '[JSON] [University]

type RegisterUniversity =
  JWTAuth
    :> Summary "Register a university"
    :> ReqBody '[JSON] NU.NewUniversity
    :> PostCreated '[JSON] University

type UniversitiesAPI =
  "universities"
    :> ( GetAllUniversities
          :<|> RegisterUniversity
          :<|> CoursesAPI
          :<|> InstructorsAPI
       )

universitiesServer :: Pool Connection -> Server UniversitiesAPI
universitiesServer conns =
  getAllUniversities
    :<|> registerUniversity
    :<|> coursesServer conns
    :<|> instructorsServer conns
 where
  getAllUniversities :: Handler [University]
  getAllUniversities = liftIO $ allUniversities conns

  registerUniversity :: AuthResult AU.AuthUser -> NU.NewUniversity -> Handler University
  registerUniversity (Authenticated authUser) newUniversity = do
    requireAdmin authUser
    result <- liftIO (try $ insertUniversity conns newUniversity :: IO (Either SqlError University))

    case result of
      Left _ -> throwError err400
      Right university -> return university
  registerUniversity _ _ = throwError err401
