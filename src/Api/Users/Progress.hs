{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users.Progress (ProgressAPI, progressServer) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations.Users (userById)
import Database.Operations.Users.Progress (userProgress)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import Types.User.Progress (Progress)
import Types.User.Role (Role (Admin))

type GetUserProgress =
  Summary "View user course progress"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "progress"
    :> Get '[JSON] [Progress]

type ProgressAPI = GetUserProgress

progressServer :: Pool Connection -> Server GetUserProgress
progressServer conns = getUserProgress
 where
  getUserProgress :: AuthResult AU.AuthUser -> Int -> Handler [Progress]
  getUserProgress (Authenticated authUser) userId = do
    ensureExists conns userById userId

    case AU.role authUser of
      Admin -> liftIO $ userProgress conns userId
      _ -> do
        unless (AU.id authUser == userId) $ throwError err401

        liftIO $ userProgress conns userId
  getUserProgress _ _ = throwError err401
