{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Essays (EssaysAPI, essaysServer) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists, ensureExistsReturning)
import Database.Operations.Courses (courseById)
import Database.Operations.Exercises.Essays (allEssays, insertEssay)
import Database.Operations.Lessons (lessonByNumber)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import Types.Exercise.Essay
import qualified Types.Exercise.NewEssay as NE
import qualified Types.User as U
import Types.User.Role (Role (Admin, Instructor, Student))

type GetAllEssays =
  Summary "Get all questions in a lesson"
    :> Get '[JSON] [Essay]

type AddEssay =
  Summary "Add a question to a lesson"
    :> JWTAuth
    :> ReqBody '[JSON] NE.NewEssay
    :> PostCreated '[JSON] Essay

type EssaysAPI =
  Capture' '[Required, Description "Number of the lesson"] "number" Int
    :> "essays"
    :> ( GetAllEssays
          :<|> AddEssay
       )

essaysServer :: Pool Connection -> Int -> Server EssaysAPI
essaysServer conns courseId lessonNumber =
  getAllEssays
    :<|> addEssay
 where
  ensureCourseExists = ensureExists conns courseById courseId
  ensureLessonExists = ensureExists conns (`lessonByNumber` courseId) lessonNumber
  getCurrentCourse = ensureExistsReturning conns courseById courseId

  getAllEssays :: Handler [Essay]
  getAllEssays = do
    ensureCourseExists
    ensureLessonExists
    liftIO $ allEssays conns courseId lessonNumber

  addEssay :: AuthResult AU.AuthUser -> NE.NewEssay -> Handler Essay
  addEssay (Authenticated authUser) newEssay =
    case AU.role authUser of
      Admin -> do
        ensureCourseExists
        ensureLessonExists
        addEssay'
      Instructor -> do
        course <- getCurrentCourse

        when (AU.id authUser /= (U.id . C.instructor $ course)) (throwError err401)

        addEssay'
      Student -> throwError err401
   where
    addEssay' = do
      res <-
        liftIO $
          try $
            insertEssay conns courseId lessonNumber newEssay ::
          Handler (Either SqlError Essay)

      case res of
        Left _ -> throwError err400
        Right question -> return question
  addEssay _ _ = throwError err401
