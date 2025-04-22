{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Avatar.Upload (API, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import LearningJunkie.Users.Database (toUserType, updateUser)
import LearningJunkie.Users.User (User)
import qualified LearningJunkie.Users.User.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.Environment (Environment (minioBucket))
import LearningJunkie.Web.JWTAuth (JWTAuth)
import LearningJunkie.Web.Minio (uploadFileMinio)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Multipart
import Servant.Multipart.OpenApi ()

type API =
    Summary "Upload new user avatar"
        :> JWTAuth
        :> MultipartForm Mem (MultipartData Mem)
        :> Post '[JSON] User

handler :: AuthResult Auth.AuthUser -> MultipartData Mem -> AppM User
handler (Authenticated authUser) multipartData = do
    let eitherFileData = lookupFile "file" multipartData

    case eitherFileData of
        Left _ -> throwError err400{errBody = "Multipart body not attached"}
        Right fileData -> do
            let
                avatarOriginalFileName = fdFileName fileData

                avatarContentType = fdFileCType fileData

                extension = last $ T.splitOn "." avatarOriginalFileName

                payload :: BS.ByteString
                payload = BS.toStrict $ fdPayload fileData

            avatarFileNameId <- toText <$> liftIO nextRandom

            let avatarFilePath = "avatars/" <> avatarFileNameId <> "." <> extension

            avatarUploadResult <- uploadFileMinio avatarFilePath avatarContentType payload

            case avatarUploadResult of
                Left e -> do
                    liftIO $ print e
                    throwError err400{errBody = "File not uploaded"}
                Right _ -> do
                    bucketUrl <- asks minioBucket

                    mbUser <-
                        updateUser
                            (Auth.id authUser)
                            Attributes.emptyEditUser{Attributes.avatar = Just (Just (bucketUrl <> "/" <> avatarFilePath))}
                            Nothing
                            Nothing

                    case mbUser of
                        Nothing -> throwError err404
                        Just user -> return . toUserType $ user
handler _ _ = throwError err401
