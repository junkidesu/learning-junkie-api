{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users (UsersAPI, usersServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Users
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.User
import Types.User.NewUser

type GetAllUsers =
    Summary "Get all users"
        :> Get '[JSON] [User]

type CreateUser =
    Summary "Create a new user"
        :> ReqBody '[JSON] NewUser
        :> PostCreated '[JSON] User

type GetUserById =
    Summary "Get user by ID"
        :> Capture' '[Required, Description "The ID of the user"] "id" Int
        :> Get '[JSON] User

type UsersAPI =
    "users"
        :> ( GetAllUsers
                :<|> CreateUser
                :<|> GetUserById
           )

usersServer :: Pool Connection -> Server UsersAPI
usersServer conns = getAllUsers :<|> createUser :<|> getUserById
  where
    getAllUsers :: Handler [User]
    getAllUsers = liftIO $ allUsers conns

    createUser :: NewUser -> Handler User
    createUser = liftIO . insertUser conns

    getUserById :: Int -> Handler User
    getUserById userId = do
        mbUser <- liftIO $ userById conns userId

        case mbUser of
            Nothing -> throwError err404
            Just user -> return user
