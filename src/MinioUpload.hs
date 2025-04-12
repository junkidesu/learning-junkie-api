{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MinioUpload where

import Configuration.Dotenv
import Network.Minio
import UnliftIO

c :: ConnectInfo
c = "http://localhost:9000"

uploadFileToMinio :: IO ()
uploadFileToMinio = do
    onMissingFile (loadFile defaultConfig) (pure ())

    creds <- setCredsFrom [fromMinioEnv, fromAWSEnv] c :: IO ConnectInfo
    let bucket = "learning-junkie-aws-bucket"

    eitherRes <- runMinio creds $ do
        bErr <- try $ makeBucket bucket Nothing
        case bErr of
            Left BucketAlreadyOwnedByYou -> return ()
            Left e -> throwIO e
            Right _ -> return ()

        -- Upload filepath to bucket; object is derived from filepath.
        fPutObject bucket "avatars/image.jpg" "image.jpg" defaultPutObjectOptions

    case eitherRes of
        Left e -> print e
        Right res -> print res
