{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises (ExercisesAPI, exercisesServer) where

import Api.Exercises.Essays (EssaysAPI, essaysServer)
import Api.Exercises.Questions (QuestionsAPI, questionsServer)
import Api.Exercises.Quizzes (QuizzesAPI, quizzesServer)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Auth.JWTAuth (JWTAuth)

type DeleteExercise =
  Summary "Delete exercise by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Verb 'DELETE 204 '[JSON] NoContent

type ExercisesAPI =
  "exercises"
    :> ( DeleteExercise
          :<|> QuestionsAPI
          :<|> EssaysAPI
          :<|> QuizzesAPI
       )

exercisesServer :: Pool Connection -> Server ExercisesAPI
exercisesServer conns =
  undefined
    :<|> questionsServer conns
    :<|> essaysServer conns
    :<|> quizzesServer conns
