{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users.Avatar (AvatarAPI, avatarServer) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Operations.Users.Avatar (deleteUserAvatar, setAvatar)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Multipart
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import Types.User (User)
import Upload (uploadFileToS3)
import Upload.Environment (S3Environment)

type UploadAvatar =
  Summary "Upload user avatar"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "avatar"
    :> MultipartForm Mem (MultipartData Mem)
    :> Post '[JSON] User

type DeleteAvatar =
  Summary "Delete user avatar"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "avatar"
    :> Verb 'DELETE 200 '[JSON] User

type AvatarAPI =
  UploadAvatar
    :<|> DeleteAvatar

avatarServer :: Pool Connection -> S3Environment -> Server AvatarAPI
avatarServer conns s3env = uploadAvatar :<|> deleteAvatar
 where
  uploadAvatar :: AuthResult AU.AuthUser -> Int -> MultipartData Mem -> Handler User
  uploadAvatar (Authenticated authUser) userId multipartData = do
    unless (AU.id authUser == userId) $ throwError err401

    let eitherFileData = lookupFile "file" multipartData

    case eitherFileData of
      Left _ -> throwError err400
      Right fileData -> do
        eitherAvatarUrl <-
          liftIO $
            uploadFileToS3
              s3env
              (fdPayload fileData)
              ("avatars/user" <> (T.pack . show $ userId))

        case eitherAvatarUrl of
          Left _ -> throwError err400
          Right avatarUrl -> do
            mbUser <- liftIO $ setAvatar conns userId avatarUrl

            case mbUser of
              Nothing -> throwError err404
              Just user -> return user
  uploadAvatar _ _ _ = throwError err401

  deleteAvatar :: AuthResult AU.AuthUser -> Int -> Handler User
  deleteAvatar (Authenticated authUser) userId = do
    unless (AU.id authUser == userId) $ throwError err401

    mbUser <- liftIO $ deleteUserAvatar conns userId

    case mbUser of
      Nothing -> throwError err404
      Just user -> return user
  deleteAvatar _ _ = throwError err401
