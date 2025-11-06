{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Logo.Upload where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import LearningJunkie.Universities.Database (toUniversityType, updateUniversity)
import LearningJunkie.Universities.University (University)
import qualified LearningJunkie.Universities.University.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.Environment (Environment (minioBucket))
import LearningJunkie.Web.JWTAuth (JWTAuth)
import LearningJunkie.Web.Minio (uploadFileMinio)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Multipart
import Servant.Multipart.OpenApi ()

type API =
    Summary "Upload new university logo"
        :> JWTAuth
        :> MultipartForm Mem (MultipartData Mem)
        :> Post '[JSON] University

handler :: Int32 -> AuthResult Auth.AuthUser -> MultipartData Mem -> AppM University
handler universityId (Authenticated authUser) multipartData = Permissions.requirePermissionWithId
    authUser
    ( Permissions.Permission
        Permissions.SameUniversity
        Permissions.Update
        Permissions.University
    )
    (Just universityId)
    $ do
        let eitherFileData = lookupFile "file" multipartData

        case eitherFileData of
            Left _ -> throwError err400{errBody = "Multipart body not attached"}
            Right fileData -> do
                let
                    logoOriginalFileName = fdFileName fileData

                    logoContentType = fdFileCType fileData

                    extension = last $ T.splitOn "." logoOriginalFileName

                    payload :: BS.ByteString
                    payload = BS.toStrict $ fdPayload fileData

                avatarFileNameId <- toText <$> liftIO nextRandom

                let logoFilePath = "logos/" <> avatarFileNameId <> "." <> extension

                logoUploadResult <- uploadFileMinio logoFilePath logoContentType payload

                case logoUploadResult of
                    Left e -> do
                        liftIO $ print e
                        throwError err400{errBody = "File not uploaded"}
                    Right _ -> do
                        bucketUrl <- asks minioBucket

                        university <-
                            updateUniversity
                                universityId
                                Attributes.emptyEditUniversity{Attributes.logo = Just (Just (bucketUrl <> "/" <> logoFilePath))}

                        return $ toUniversityType university
handler _ _ _ = throwError err401
