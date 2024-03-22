{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Exercise (ExerciseAPI, exerciseServer) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple (SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise as E
import qualified Types.User as U
import Types.User.Role (Role (..))

type GetExerciseById ex =
  Summary "Get exercise by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] ex

type EditExercise ex editEx =
  Summary "Edit exercise by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] editEx
    :> Put '[JSON] ex

type ExerciseAPI ex editEx = GetExerciseById ex :<|> EditExercise ex editEx

exerciseServer ::
  (Int -> IO (Maybe ex)) ->
  (Int -> editEx -> IO (Maybe ex)) ->
  (Int -> Handler E.Exercise) ->
  Server
    (ExerciseAPI ex editEx)
exerciseServer exerciseById updateExercise ensureExerciseExists =
  getExerciseById exerciseById
    :<|> editExerciseById (\foo bar -> liftIO $ try $ updateExercise foo bar) ensureExerciseExists

getExerciseById :: (Int -> IO (Maybe ex)) -> Int -> Handler ex
getExerciseById exerciseById exerciseId = do
  mbExercise <- liftIO $ exerciseById exerciseId

  case mbExercise of
    Nothing -> throwError err404
    Just exercise -> return exercise

editExerciseById ::
  (Int -> editEx -> Handler (Either SqlError (Maybe ex))) ->
  (Int -> Handler E.Exercise) ->
  AuthResult AU.AuthUser ->
  Int ->
  editEx ->
  Handler ex
editExerciseById updateExercise ensureExerciseExists (Authenticated authUser) exerciseId editExercise =
  case AU.role authUser of
    Admin -> do
      res <- updateExercise exerciseId editExercise

      case res of
        Left _ -> throwError err400
        Right mbExercise -> case mbExercise of
          Nothing -> throwError err404
          Just exercise -> return exercise
    Instructor -> do
      ex <- ensureExerciseExists exerciseId

      when
        (AU.id authUser /= (U.id . C.instructor . E.course $ ex))
        $ throwError err401

      res <- updateExercise exerciseId editExercise

      case res of
        Left _ -> throwError err400
        Right mbExercise -> case mbExercise of
          Nothing -> throwError err404
          Just exercise -> return exercise
    Student -> throwError err401
editExerciseById _ _ _ _ _ = throwError err401
