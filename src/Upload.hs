{-# LANGUAGE OverloadedStrings #-}

module Upload (uploadFileToS3) where

import Aws.S3 (multipartUploadSink)
import Conduit
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Binary (sourceLbs)
import qualified Data.Text as T
import Upload.Environment (S3Environment (S3Environment))

uploadFileToS3 ::
    S3Environment ->
    LBS.ByteString ->
    T.Text ->
    IO (Either String T.Text)
uploadFileToS3 (S3Environment cfg s3cfg mgr) fileBS filename = do
    res <-
        try $
            runResourceT $
                sourceLbs fileBS
                    `connect` multipartUploadSink cfg s3cfg mgr bucket filename (10 * 1024 * 1024) ::
            IO (Either SomeException ())

    case res of
        Left _ -> return $ Left "could not upload file"
        Right _ -> return $ Right uploadedFileUrl
  where
    bucket :: T.Text
    bucket = "learning-junkie-aws-bucket"

    endpoint :: T.Text
    endpoint = "s3.us-east-1.amazonaws.com"

    uploadedFileUrl :: T.Text
    uploadedFileUrl = "https://" <> bucket <> "." <> endpoint <> "/" <> filename
