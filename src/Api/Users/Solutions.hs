{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users.Solutions (SolutionsAPI, solutionsServer) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations.Users (userById)
import Database.Operations.Users.Solutions (userSolutions)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import Types.Exercise (Exercise)
import Types.User.Role (Role (Admin))

type GetUserSolutions =
  Summary "Get exercises solved by the user"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "solutions"
    :> Get '[JSON] [Exercise]

type SolutionsAPI = GetUserSolutions

solutionsServer :: Pool Connection -> Server SolutionsAPI
solutionsServer conns = getUserSolutions
 where
  getUserSolutions :: AuthResult AU.AuthUser -> Int -> Handler [Exercise]
  getUserSolutions (Authenticated authUser) userId = do
    ensureExists conns userById userId
    case AU.role authUser of
      Admin -> do
        liftIO $ userSolutions conns userId
      _ -> do
        unless (AU.id authUser == userId) $ throwError err401

        liftIO $ userSolutions conns userId
  getUserSolutions _ _ = throwError err401
