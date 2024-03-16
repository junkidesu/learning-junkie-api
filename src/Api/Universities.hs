{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities (UniversitiesAPI, universitiesServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Courses (insertCourse, universityCourses)
import Database.Operations.Universities (allUniversities, insertUniversity)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth
import Servant.Auth.Server
import qualified Types.Auth.User as AU
import Types.Course (Course)
import qualified Types.Course.NewCourse as NC
import Types.University
import qualified Types.University.NewUniversity as NU
import Types.User.Role (Role (Admin))

type JWTAuth = Auth '[JWT] AU.AuthUser

type GetAllUniversities =
  Summary "Get all universities"
    :> Get '[JSON] [University]

type RegisterUniversity =
  JWTAuth
    :> Summary "Register a university"
    :> ReqBody '[JSON] NU.NewUniversity
    :> PostCreated '[JSON] University

type AddCourseById =
  Summary "Add course to the university"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the university"] "id" Int
    :> "courses"
    :> ReqBody '[JSON] NC.NewCourse
    :> PostCreated '[JSON] Course

type UniversityCoursesById =
  Summary "Get courses by university with given ID"
    :> Capture' '[Required, Description "ID of the university"] "id" Int
    :> "courses"
    :> Get '[JSON] [Course]

type UniversitiesAPI = "universities" :> (GetAllUniversities :<|> RegisterUniversity :<|> AddCourseById :<|> UniversityCoursesById)

universitiesServer :: Pool Connection -> Server UniversitiesAPI
universitiesServer conns =
  getAllUniversities
    :<|> registerUniversity
    :<|> addCourseById
    :<|> universityCoursesById
 where
  getAllUniversities :: Handler [University]
  getAllUniversities = liftIO $ allUniversities conns

  registerUniversity :: AuthResult AU.AuthUser -> NU.NewUniversity -> Handler University
  registerUniversity (Authenticated authUser) newUniversity =
    case AU.role authUser of
      Admin -> liftIO $ insertUniversity conns newUniversity
      _ -> throwError err401
  registerUniversity _ _ = throwError err401

  addCourseById :: AuthResult AU.AuthUser -> Int -> NC.NewCourse -> Handler Course
  addCourseById (Authenticated authUser) universityId newCourse =
    case AU.role authUser of
      Admin -> liftIO $ insertCourse conns universityId newCourse
      _ -> throwError err401
  addCourseById _ _ _ = throwError err401

  universityCoursesById :: Int -> Handler [Course]
  universityCoursesById = liftIO <$> universityCourses conns
