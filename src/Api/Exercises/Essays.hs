{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Essays (EssaysAPI, essaysServer) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Exercises (exerciseById)
import Database.Operations.Exercises.Essays (essayById, updateEssay)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise as E
import qualified Types.Exercise.EditEssay as EE
import Types.Exercise.Essay (Essay)
import qualified Types.User as U
import Types.User.Role (Role (Admin, Instructor, Student))

type GetEssayById =
  Summary "Get essay by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Essay

type EditEssay =
  Summary "Edit essay with given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] EE.EditEssay
    :> Put '[JSON] Essay

type EssaysAPI =
  "essays"
    :> (GetEssayById :<|> EditEssay)

essaysServer :: Pool Connection -> Server EssaysAPI
essaysServer conns = getEssayById :<|> editEssay
 where
  getEssayById :: Int -> Handler Essay
  getEssayById exerciseId = do
    mbEssay <- liftIO $ essayById conns exerciseId

    case mbEssay of
      Nothing -> throwError err404
      Just essay -> return essay

  editEssay :: AuthResult AU.AuthUser -> Int -> EE.EditEssay -> Handler Essay
  editEssay (Authenticated authUser) exerciseId ee =
    case AU.role authUser of
      Admin -> editEssay'
      Instructor -> do
        exercise <- ensureExistsReturning conns exerciseById exerciseId

        when (AU.id authUser /= (U.id . C.instructor . E.course $ exercise)) (throwError err401)

        editEssay'
      Student -> throwError err401
   where
    editEssay' = do
      res <-
        liftIO $
          try $
            updateEssay conns exerciseId ee ::
          Handler (Either SqlError (Maybe Essay))

      case res of
        Left _ -> throwError err400
        Right mbEssay ->
          case mbEssay of
            Nothing -> throwError err404
            Just essay -> return essay
  editEssay _ _ _ = throwError err401
