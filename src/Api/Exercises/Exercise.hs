{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Exercise (ExerciseAPI, exerciseServer) where

import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Courses.Completions (courseCompletion, insertCompletion)
import Database.Operations.Courses.Enrollments (userIsEnrolled)
import Database.Operations.Exercises (exerciseById)
import Database.Operations.Exercises.Solutions (insertSolution, userDidSolve)
import Database.Operations.Users.Progress (userCourseProgress)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise as E
import Types.Solution.ExerciseCheck
import Types.Solution.Response (Response (Response))
import Types.Solvable (Solvable (checkSolution))
import qualified Types.User as U
import Types.User.Progress (courseFinished)
import Types.User.Role (Role (..))

type GetExerciseById ex =
  Summary "Get exercise of given kind by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] ex

type EditExercise ex editEx =
  Summary "Edit exercise of given kind by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] editEx
    :> Put '[JSON] ex

type PostSolution sol =
  Summary "Post solution to exercise of given kind"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> ReqBody '[JSON] sol
    :> Post '[JSON] Response

type GetSolution sol =
  Summary "View solution to the exercise of given kind"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> Get '[JSON] sol

type ExerciseAPI ex editEx sol =
  GetExerciseById ex
    :<|> EditExercise ex editEx
    :<|> PostSolution sol
    :<|> GetSolution sol

exerciseServer ::
  (Solvable ex sol) =>
  Pool Connection ->
  (Pool Connection -> Int -> IO (Maybe ex)) ->
  (Pool Connection -> Int -> editEx -> IO (Maybe ex)) ->
  (Pool Connection -> Int -> IO (Maybe sol)) ->
  Server
    (ExerciseAPI ex editEx sol)
exerciseServer
  conns
  exerciseById'
  updateExercise
  solutionById =
    getExerciseById conns exerciseById'
      :<|> editExerciseById
        conns
        (\conns' bar baz -> liftIO . try . updateExercise conns' bar $ baz)
        getCurrentExercise
      :<|> postSolution
        conns
        exerciseById'
        getCurrentExercise
      :<|> getSolution conns solutionById
   where
    getCurrentExercise = ensureExistsReturning conns exerciseById

getExerciseById :: Pool Connection -> (Pool Connection -> Int -> IO (Maybe ex)) -> Int -> Handler ex
getExerciseById conns exerciseById' exerciseId = do
  mbExercise <- liftIO $ exerciseById' conns exerciseId

  case mbExercise of
    Nothing -> throwError err404
    Just exercise -> return exercise

editExerciseById ::
  Pool Connection ->
  (Pool Connection -> Int -> editEx -> Handler (Either SqlError (Maybe ex))) ->
  (Int -> Handler E.Exercise) ->
  AuthResult AU.AuthUser ->
  Int ->
  editEx ->
  Handler ex
editExerciseById
  conns
  updateExercise
  getCurrentExercise
  (Authenticated authUser)
  exerciseId
  editExercise =
    case AU.role authUser of
      Admin -> do
        res <- updateExercise conns exerciseId editExercise

        case res of
          Left _ -> throwError err400
          Right mbExercise -> case mbExercise of
            Nothing -> throwError err404
            Just exercise -> return exercise
      Instructor -> do
        currentExercise <- getCurrentExercise exerciseId

        when
          (AU.id authUser /= (U.id . C.instructor . E.course $ currentExercise))
          $ throwError err401

        res <- updateExercise conns exerciseId editExercise

        case res of
          Left _ -> throwError err400
          Right mbExercise -> case mbExercise of
            Nothing -> throwError err404
            Just exercise -> return exercise
      Student -> throwError err401
editExerciseById _ _ _ _ _ _ = throwError err401

getSolution :: Pool Connection -> (Pool Connection -> Int -> IO (Maybe sol)) -> AuthResult AU.AuthUser -> Int -> Handler sol
getSolution conns solutionById (Authenticated authUser) exerciseId =
  case AU.role authUser of
    Admin -> do
      mbSolution <- liftIO $ solutionById conns exerciseId

      case mbSolution of
        Nothing -> throwError err404
        Just solution -> return solution
    _ -> do
      userSolved <- liftIO $ userDidSolve conns (AU.id authUser) exerciseId

      unless userSolved $ throwError err401

      mbSolution <- liftIO $ solutionById conns exerciseId

      case mbSolution of
        Nothing -> throwError err404
        Just solution -> return solution
getSolution _ _ _ _ = undefined

postSolution ::
  (Solvable ex sol) =>
  Pool Connection ->
  (Pool Connection -> Int -> IO (Maybe ex)) ->
  (Int -> Handler E.Exercise) ->
  AuthResult AU.AuthUser ->
  Int ->
  sol ->
  Handler Response
postSolution
  conns
  exerciseById'
  getCurrentExercise
  (Authenticated authUser)
  exerciseId
  solution = do
    mbExercise <- liftIO $ exerciseById' conns exerciseId

    case mbExercise of
      Nothing -> throwError err404
      Just exercise -> do
        let res = checkSolution exercise solution

        currentExercise <- getCurrentExercise exerciseId

        let courseId = C.id . E.course $ currentExercise
            userId = AU.id authUser

        isEnrolled <- liftIO $ userIsEnrolled conns courseId userId

        unless isEnrolled (throwError err401)

        case res of
          ExerciseFailure -> return $ Response ExerciseFailure Nothing
          ExercisePending -> return $ Response ExercisePending Nothing
          ExerciseSuccess -> do
            grade <-
              liftIO $
                insertSolution
                  conns
                  (AU.id authUser)
                  exerciseId
                  (E.grade currentExercise)

            mbCompletion <- liftIO $ courseCompletion conns courseId userId

            case mbCompletion of
              Nothing -> do
                mbProgress <- liftIO $ userCourseProgress conns userId courseId

                case mbProgress of
                  Nothing -> throwError err404
                  Just progress -> when (courseFinished progress) (liftIO $ insertCompletion conns courseId userId)
              Just _ -> return ()

            return $ Response ExerciseSuccess (Just grade)
postSolution _ _ _ _ _ _ = throwError err401
