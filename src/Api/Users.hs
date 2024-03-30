{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users (UsersAPI, usersServer) where

import Api.Users.Avatar (AvatarAPI, avatarServer)
import Api.Users.Courses (CoursesAPI, coursesServer)
import Api.Users.Progress (ProgressAPI, progressServer)
import Api.Users.Solutions (SolutionsAPI, solutionsServer)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Users
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth
import qualified Types.Auth.User as AU
import Types.User
import qualified Types.User.NewUser as NU
import Types.User.Role (Role (Admin))
import Upload.Environment (S3Environment)

type GetAllUsers =
    Summary "Get all users"
        :> Get '[JSON] [User]

type CreateUser =
    Summary "Create a new user"
        :> Description "Only prospective students can use this endpoint"
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
                :<|> CoursesAPI
                :<|> SolutionsAPI
                :<|> ProgressAPI
                :<|> AvatarAPI
           )

usersServer :: Pool Connection -> S3Environment -> Server UsersAPI
usersServer conns s3env =
    getAllUsers
        :<|> createUser
        :<|> getUserById
        :<|> deleteUserById
        :<|> coursesServer conns
        :<|> solutionsServer conns
        :<|> progressServer conns
        :<|> avatarServer conns s3env
  where
    getAllUsers :: Handler [User]
    getAllUsers = liftIO $ allUsers conns

    createUser :: NU.NewUser -> Handler User
    createUser newUser = do
        result <-
            liftIO $
                try $
                    insertUser conns newUser ::
                Handler (Either SqlError User)

        case result of
            Left _ -> throwError err400
            Right user -> return user

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
