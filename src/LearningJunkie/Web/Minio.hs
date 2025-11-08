{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Web.Minio where

import Conduit (sourceToList, yield)
import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Environment (Environment (bucket, env, minioConnection), HaskellEnv (Development, Production))
import Network.HTTP.Client
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.Minio

connectMinio :: HaskellEnv -> IO MinioConn
connectMinio currentEnv =
  do
    onMissingFile (loadFile defaultConfig) (putStrLn "Missing environment variables")

    connectInfo <- case currentEnv of
      Production -> setRegion "eu-north-1" <$> setCredsFrom [fromAWSEnv] awsCI
      Development -> setCredsFrom [fromMinioEnv] "http://localhost:9000"

    manager <- newManager tlsManagerSettings

    mkMinioConn
      connectInfo
      manager

uploadFileMinio :: T.Text -> T.Text -> BS.ByteString -> AppM (Either MinioErr T.Text)
uploadFileMinio filePath fileCType file = do
  minioConn <- asks minioConnection
  bn <- asks bucket
  currentEnv <- asks env

  eitherRes <-
    liftIO $
      runMinioWith minioConn $
        putObject
          bn
          filePath
          (yield file)
          Nothing
          defaultPutObjectOptions{pooContentType = Just fileCType}

  case eitherRes of
    Left e -> pure $ Left e
    Right _ -> do
      case currentEnv of
        Development -> do
          pure $ Right $ "http://localhost:9000" <> "/" <> bn <> "/" <> filePath
        Production -> do
          pure $ Right $ "https://learning-junkie-bucket.s3.amazonaws.com" <> "/" <> filePath

getFileMinio :: T.Text -> AppM (Either MinioErr BS.ByteString)
getFileMinio objectName = do
  minioConn <- asks minioConnection
  bucketName <- asks bucket

  liftIO $ do
    runMinioWith minioConn $ do
      gor <-
        getObject
          bucketName
          objectName
          defaultGetObjectOptions
      result <- sourceToList $ gorObjectStream gor

      return $ head result
