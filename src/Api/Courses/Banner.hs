{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Banner (BannerAPI, bannerServer) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database (ensureExistsReturning)
import Database.Operations.Courses (courseById)
import Database.Operations.Courses.Banner (deleteCourseBanner, setBanner)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Multipart
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import Types.Course (Course (instructor))
import qualified Types.User as U
import Types.User.Role (Role (Admin, Instructor, Student))
import Upload (uploadFileToS3)
import Upload.Environment (S3Environment)

type UploadBanner =
  Summary "Upload course banner"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the course"] "id" Int
    :> "banner"
    :> MultipartForm Mem (MultipartData Mem)
    :> Post '[JSON] Course

type DeleteBanner =
  Summary "Delete course banner"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the course"] "id" Int
    :> "banner"
    :> Verb 'DELETE 200 '[JSON] Course

type BannerAPI =
  UploadBanner
    :<|> DeleteBanner

bannerServer :: Pool Connection -> S3Environment -> Server BannerAPI
bannerServer conns s3env = uploadBanner :<|> deleteBanner
 where
  uploadBanner :: AuthResult AU.AuthUser -> Int -> MultipartData Mem -> Handler Course
  uploadBanner (Authenticated authUser) courseId multipartData =
    case AU.role authUser of
      Admin -> do
        let eitherFileData = lookupFile "file" multipartData

        case eitherFileData of
          Left _ -> throwError err400
          Right fileData -> do
            eitherBannerUrl <-
              liftIO $
                try $
                  uploadFileToS3
                    s3env
                    (fdPayload fileData)
                    ("courses/banners/course" <> (T.pack . show $ courseId)) ::
                Handler (Either SomeException T.Text)

            case eitherBannerUrl of
              Left _ -> throwError err400
              Right bannerUrl -> do
                mbCourse <- liftIO $ setBanner conns courseId bannerUrl

                case mbCourse of
                  Nothing -> throwError err404
                  Just course -> return course
      Instructor -> do
        currentCourse <- ensureExistsReturning conns courseById courseId

        unless ((U.id . instructor $ currentCourse) == AU.id authUser) $ throwError err401

        let eitherFileData = lookupFile "file" multipartData

        case eitherFileData of
          Left _ -> throwError err400
          Right fileData -> do
            eitherBannerUrl <-
              liftIO $
                try $
                  uploadFileToS3
                    s3env
                    (fdPayload fileData)
                    ("courses/banners/course" <> (T.pack . show $ courseId)) ::
                Handler (Either SomeException T.Text)

            case eitherBannerUrl of
              Left _ -> throwError err400
              Right bannerUrl -> do
                mbCourse <- liftIO $ setBanner conns courseId bannerUrl

                case mbCourse of
                  Nothing -> throwError err404
                  Just course -> return course
      Student -> throwError err401
  uploadBanner _ _ _ = throwError err401

  deleteBanner :: AuthResult AU.AuthUser -> Int -> Handler Course
  deleteBanner (Authenticated authUser) courseId = do
    case AU.role authUser of
      Admin -> do
        mbCourse <- liftIO $ deleteCourseBanner conns courseId

        case mbCourse of
          Nothing -> throwError err404
          Just course -> return course
      Instructor -> do
        currentCourse <- ensureExistsReturning conns courseById courseId

        unless ((U.id . instructor $ currentCourse) == AU.id authUser) $ throwError err401

        mbCourse <- liftIO $ deleteCourseBanner conns courseId

        case mbCourse of
          Nothing -> throwError err404
          Just course -> return course
      _ -> throwError err401
  deleteBanner _ _ = throwError err401
