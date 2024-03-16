{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users (UsersAPI, usersServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Users
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth
import Servant.Auth.Server
import qualified Types.Auth.User as AU
import Types.User
import qualified Types.User.NewUser as NU
import Types.User.Role (Role (Admin))

type JWTAuth = Auth '[JWT] AU.AuthUser

type GetAllUsers =
    Summary "Get all users"
        :> Get '[JSON] [User]

type CreateUser =
    Summary "Create a new user"
        :> ReqBody '[JSON] NU.NewUser
        :> PostCreated '[JSON] User

type GetUserById =
    Summary "Get user by ID"
        :> Capture' '[Required, Description "ID of the user"] "id" Int
        :> Get '[JSON] User

type DeleteUserById =
    Summary "Delete user with the given ID"
        :> JWTAuth
        :> Capture' '[Required, Description "ID of the user"] "id" Int
        :> Verb 'DELETE 204 '[JSON] NoContent

type UsersAPI =
    "users"
        :> ( GetAllUsers
                :<|> CreateUser
                :<|> GetUserById
                :<|> DeleteUserById
           )

usersServer :: Pool Connection -> Server UsersAPI
usersServer conns =
    getAllUsers
        :<|> createUser
        :<|> getUserById
        :<|> deleteUserById
  where
    getAllUsers :: Handler [User]
    getAllUsers = liftIO $ allUsers conns

    createUser :: NU.NewUser -> Handler User
    createUser newUser = case NU.role newUser of
        Admin -> throwError err400
        _ -> liftIO $ insertUser conns newUser

    getUserById :: Int -> Handler User
    getUserById userId = do
        mbUser <- liftIO $ userById conns userId

        case mbUser of
            Nothing -> throwError err404
            Just user -> return user
    deleteUserById :: AuthResult AU.AuthUser -> Int -> Handler NoContent
    deleteUserById (Authenticated authUser) userId =
        case AU.role authUser of
            Admin -> do
                liftIO $ deleteUser conns userId
                return NoContent
            _ ->
                if AU.id authUser /= userId
                    then throwError err401
                    else do
                        liftIO $ deleteUser conns userId
                        return NoContent
    deleteUserById _ _ = throwError err401
