{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Essays (EssaysAPI, essaysServer) where

import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Courses.Completions (courseCompletion, insertCompletion)
import Database.Operations.Courses.Enrollments (userIsEnrolled)
import Database.Operations.Exercises.Essays (essayById, updateEssay)
import Database.Operations.Exercises.Solutions (essayModelSolution, insertSolution, userDidSolve)
import Database.Operations.Users.Progress (userCourseProgress)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise.EditEssay as EE
import qualified Types.Exercise.Essay as E
import Types.Solution.Essay (EssaySolution)
import qualified Types.Solution.Essay as ES
import Types.Solution.ExerciseCheck
import Types.Solution.Response (Response (Response))
import Types.Solvable (Solvable (checkSolution))
import qualified Types.User as U
import Types.User.Progress (courseFinished)
import Types.User.Role (Role (..))

type GetEssayById =
  Summary "Get essay ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] E.Essay

type EditEssay =
  Summary "Edit essay by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] EE.EditEssay
    :> Put '[JSON] E.Essay

type PostSolution =
  Summary "Post solution to essay"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> ReqBody '[JSON] EssaySolution
    :> Post '[JSON] Response

type GetSolution =
  Summary "View solution to essay"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> Get '[JSON] EssaySolution

type EssaysAPI =
  GetEssayById
    :<|> EditEssay
    :<|> PostSolution
    :<|> GetSolution

essaysServer :: Pool Connection -> Server EssaysAPI
essaysServer conns =
  getEssayById
    :<|> editEssayById
    :<|> postSolution
    :<|> getSolution
 where
  getEssayById :: Int -> Handler E.Essay
  getEssayById essayId = do
    mbEssay <- liftIO $ essayById conns essayId

    case mbEssay of
      Nothing -> throwError err404
      Just essay -> return essay

  editEssayById :: AuthResult AU.AuthUser -> Int -> EE.EditEssay -> Handler E.Essay
  editEssayById (Authenticated authUser) essayId editEssay =
    case AU.role authUser of
      Admin -> do
        res <- liftIO $ try $ updateEssay conns essayId editEssay :: Handler (Either SqlError (Maybe E.Essay))

        case res of
          Left _ -> throwError err400
          Right mbEssay -> case mbEssay of
            Nothing -> throwError err404
            Just essay -> return essay
      Instructor -> do
        currentEssay <- ensureExistsReturning conns essayById essayId

        when
          (AU.id authUser /= (U.id . C.instructor . E.course $ currentEssay))
          $ throwError err401

        res <- liftIO $ try $ updateEssay conns essayId editEssay :: Handler (Either SqlError (Maybe E.Essay))

        case res of
          Left _ -> throwError err400
          Right mbEssay -> case mbEssay of
            Nothing -> throwError err404
            Just essay -> return essay
      Student -> throwError err401
  editEssayById _ _ _ = throwError err401

  getSolution :: AuthResult AU.AuthUser -> Int -> Handler ES.EssaySolution
  getSolution (Authenticated authUser) essayId =
    case AU.role authUser of
      Admin -> do
        mbSolution <- liftIO $ essayModelSolution conns essayId

        case mbSolution of
          Nothing -> throwError err404
          Just solution -> return solution
      _ -> do
        userSolved <- liftIO $ userDidSolve conns (AU.id authUser) essayId

        unless userSolved $ throwError err401

        mbSolution <- liftIO $ essayModelSolution conns essayId

        case mbSolution of
          Nothing -> throwError err404
          Just solution -> return solution
  getSolution _ _ = throwError err401

  postSolution :: AuthResult AU.AuthUser -> Int -> ES.EssaySolution -> Handler Response
  postSolution (Authenticated authUser) essayId solution = do
    essay <- ensureExistsReturning conns essayById essayId

    let res = checkSolution essay solution
        courseId = C.id . E.course $ essay
        userId = AU.id authUser

    isEnrolled <- liftIO $ userIsEnrolled conns courseId userId

    unless isEnrolled (throwError err401)

    case res of
      ExerciseSuccess -> do
        grade <-
          liftIO $
            insertSolution
              conns
              (AU.id authUser)
              essayId
              (E.grade essay)

        mbCompletion <- liftIO $ courseCompletion conns courseId userId

        case mbCompletion of
          Nothing -> do
            mbProgress <- liftIO $ userCourseProgress conns userId courseId

            case mbProgress of
              Nothing -> throwError err404
              Just progress -> when (courseFinished progress) (liftIO $ insertCompletion conns courseId userId)
          Just _ -> return ()

        return $ Response ExerciseSuccess (Just grade)
      ExerciseFailure -> return $ Response ExerciseFailure Nothing
      ExercisePending -> return $ Response ExercisePending Nothing
  postSolution _ _ _ = throwError err401
