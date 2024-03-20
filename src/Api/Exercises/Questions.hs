{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Questions (QuestionsAPI, questionsServer) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Exercise.NewQuestion as NQ
import Types.Exercise.Question (Question)

type GetExerciseById =
  Summary "Get question by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Question

type EditQuestion =
  Summary "Edit question with given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] NQ.NewQuestion
    :> Put '[JSON] Question

type QuestionsAPI =
  "questions"
    :> (GetExerciseById :<|> EditQuestion)

questionsServer :: Pool Connection -> Server QuestionsAPI
questionsServer conns = undefined
