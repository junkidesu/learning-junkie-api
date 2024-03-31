{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities.Logo (LogoAPI, logoServer) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Operations.Universities.Logo (deleteUniversityLogo, setLogo)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Multipart
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import Types.University (University)
import Types.User.Role (Role (Admin))
import Upload (uploadFileToS3)
import Upload.Environment (S3Environment)

type UploadLogo =
  Summary "Upload university logo"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the university"] "id" Int
    :> "logo"
    :> MultipartForm Mem (MultipartData Mem)
    :> Post '[JSON] University

type DeleteLogo =
  Summary "Delete university logo"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the university"] "id" Int
    :> "logo"
    :> Verb 'DELETE 200 '[JSON] University

type LogoAPI =
  UploadLogo
    :<|> DeleteLogo

logoServer :: Pool Connection -> S3Environment -> Server LogoAPI
logoServer conns s3env = uploadLogo :<|> deleteLogo
 where
  uploadLogo :: AuthResult AU.AuthUser -> Int -> MultipartData Mem -> Handler University
  uploadLogo (Authenticated authUser) universityId multipartData =
    case AU.role authUser of
      Admin -> do
        let eitherFileData = lookupFile "file" multipartData

        case eitherFileData of
          Left _ -> throwError err400
          Right fileData -> do
            eitherLogoUrl <-
              liftIO $
                try $
                  uploadFileToS3
                    s3env
                    (fdPayload fileData)
                    ("universities/logos/university" <> (T.pack . show $ universityId)) ::
                Handler (Either SomeException T.Text)

            case eitherLogoUrl of
              Left _ -> throwError err400
              Right logoUrl -> do
                mbUniversity <- liftIO $ setLogo conns universityId logoUrl

                case mbUniversity of
                  Nothing -> throwError err404
                  Just university -> return university
      _ -> throwError err401
  uploadLogo _ _ _ = throwError err401

  deleteLogo :: AuthResult AU.AuthUser -> Int -> Handler University
  deleteLogo (Authenticated authUser) universityId = do
    case AU.role authUser of
      Admin -> do
        mbUniversity <- liftIO $ deleteUniversityLogo conns universityId

        case mbUniversity of
          Nothing -> throwError err404
          Just university -> return university
      _ -> throwError err401
  deleteLogo _ _ = throwError err401
