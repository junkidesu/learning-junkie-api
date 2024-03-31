{-# LANGUAGE OverloadedStrings #-}

module Upload (uploadFileToS3) where

import Aws.S3 (multipartUpload)
import Conduit
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Binary (sourceLbs)
import qualified Data.Text as T
import Upload.Environment (S3Environment (S3Environment))

uploadFileToS3 ::
    S3Environment ->
    LBS.ByteString ->
    T.Text ->
    IO T.Text
uploadFileToS3 (S3Environment cfg s3cfg mgr) fileBS filename = do
    runResourceT $
        multipartUpload cfg s3cfg mgr bucket filename (sourceLbs fileBS) (10 * 1024 * 1024)

    return uploadedFileUrl
  where
    bucket :: T.Text
    bucket = "learning-junkie-bucket"

    endpoint :: T.Text
    endpoint = "s3.eu-north-1.amazonaws.com"

    uploadedFileUrl :: T.Text
    uploadedFileUrl = "https://" <> bucket <> "." <> endpoint <> "/" <> filename
