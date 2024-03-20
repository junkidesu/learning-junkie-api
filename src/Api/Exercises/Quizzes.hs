{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Quizzes (QuizzesAPI, quizzesServer) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Exercise.NewQuiz as NQ
import Types.Exercise.Quiz (Quiz)

type GetQuizById =
  Summary "Get quiz by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Quiz

type EditQuiz =
  Summary "Edit quiz with given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] NQ.NewQuiz
    :> Put '[JSON] Quiz

type QuizzesAPI =
  "quizzes"
    :> (GetQuizById :<|> EditQuiz)

quizzesServer :: Pool Connection -> Server QuizzesAPI
quizzesServer conns = undefined
