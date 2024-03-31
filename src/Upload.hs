{-# LANGUAGE OverloadedStrings #-}

module Upload (uploadFileToS3) where

import Aws (dbgConfiguration)
import Aws.Core (Protocol (HTTPS))
import Aws.S3 (S3SignPayloadMode (SignWithEffort), multipartUploadSink, s3v4)
import Conduit
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Binary (sourceLbs)
import qualified Data.Text as T
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Upload.Environment (S3Environment (S3Environment))

uploadFileToS3 ::
    S3Environment ->
    LBS.ByteString ->
    T.Text ->
    IO (Either String T.Text)
uploadFileToS3 (S3Environment _ _ _) fileBS filename = do
    cfg <- dbgConfiguration
    mgr <- newManager tlsManagerSettings

    let s3cfg = s3v4 HTTPS "s3.eu-north-1.amazonaws.com" False SignWithEffort

    res <-
        try $
            runResourceT $
                sourceLbs fileBS
                    `connect` multipartUploadSink cfg s3cfg mgr bucket filename (10 * 1024 * 1024) ::
            IO (Either SomeException ())

    case res of
        Left e -> do
            print e
            return $ Left (show e)
        Right _ -> return $ Right uploadedFileUrl
  where
    bucket :: T.Text
    bucket = "learning-junkie-bucket"

    endpoint :: T.Text
    endpoint = "s3.eu-north-1.amazonaws.com"

    uploadedFileUrl :: T.Text
    uploadedFileUrl = "https://" <> bucket <> "." <> endpoint <> "/" <> filename
