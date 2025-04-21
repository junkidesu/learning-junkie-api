{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Web.Minio where

import Conduit (yield)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Environment (Environment (minioConnection))
import Network.HTTP.Client
import Network.Minio

connectMinio :: IO MinioConn
connectMinio =
  do
    connectInfo <- setCredsFrom [fromMinioEnv] "http://localhost:9000" -- we shall use localhost for now - no longer using AWS S3
    manager <- newManager defaultManagerSettings

    mkMinioConn
      connectInfo
      manager

uploadFileMinio :: T.Text -> T.Text -> BS.ByteString -> AppM (Either MinioErr ())
uploadFileMinio filePath fileCType file = do
  minioConn <- asks minioConnection

  liftIO $
    runMinioWith minioConn $
      putObject
        "learning-junkie-aws-bucket"
        filePath
        (yield file)
        Nothing
        defaultPutObjectOptions{pooContentType = Just fileCType}
