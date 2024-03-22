{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises (ExercisesAPI, exercisesServer) where

import Api.Exercises.Exercise (ExerciseAPI, exerciseServer)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Exercises (deleteExercise, exerciseById)
import Database.Operations.Exercises.Essays (essayById, updateEssay)
import Database.Operations.Exercises.Questions (questionById, updateQuestion)
import Database.Operations.Exercises.Quizzes (quizById, updateQuiz)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import Types.Exercise (Exercise)
import qualified Types.Exercise as E
import Types.Exercise.EditEssay (EditEssay)
import Types.Exercise.EditQuestion (EditQuestion)
import Types.Exercise.EditQuiz (EditQuiz)
import Types.Exercise.Essay (Essay)
import Types.Exercise.Question (Question)
import Types.Exercise.Quiz (Quiz)
import qualified Types.User as U
import Types.User.Role (Role (Admin, Instructor, Student))

type GetExerciseById =
  Summary "Get exercise by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Exercise

type DeleteExercise =
  Summary "Delete exercise by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Verb 'DELETE 204 '[JSON] NoContent

type QuestionsAPI = ExerciseAPI Question EditQuestion

type EssaysAPI = ExerciseAPI Essay EditEssay

type QuizzesAPI = ExerciseAPI Quiz EditQuiz

type ExercisesAPI =
  ( "exercises"
      :> ( GetExerciseById
            :<|> DeleteExercise
         )
      :<|> "questions" :> QuestionsAPI
      :<|> "essays" :> EssaysAPI
      :<|> "quizzes" :> QuizzesAPI
  )

exercisesServer :: Pool Connection -> Server ExercisesAPI
exercisesServer conns =
  ( getExerciseById
      :<|> deleteExerciseById
  )
    :<|> questionsServer
    :<|> essaysServer
    :<|> quizzesServer
 where
  getExerciseById :: Int -> Handler Exercise
  getExerciseById exerciseId = do
    mbExercise <- liftIO $ exerciseById conns exerciseId

    case mbExercise of
      Nothing -> throwError err404
      Just exercise -> return exercise
  deleteExerciseById :: AuthResult AU.AuthUser -> Int -> Handler NoContent
  deleteExerciseById (Authenticated authUser) exerciseId =
    case AU.role authUser of
      Admin -> do
        liftIO $
          deleteExercise
            conns
            exerciseId
        return NoContent
      Instructor -> do
        exercise <- ensureExerciseExists exerciseId

        when (AU.id authUser /= (U.id . C.instructor . E.course $ exercise)) (throwError err401)

        liftIO $ deleteExercise conns exerciseId
        return NoContent
      Student -> throwError err401
  deleteExerciseById _ _ = throwError err401

  ensureExerciseExists = ensureExistsReturning conns exerciseById
  questionsServer = exerciseServer (questionById conns) (updateQuestion conns) ensureExerciseExists
  essaysServer = exerciseServer (essayById conns) (updateEssay conns) ensureExerciseExists
  quizzesServer = exerciseServer (quizById conns) (updateQuiz conns) ensureExerciseExists
