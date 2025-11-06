{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Web (startApp) where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import LearningJunkie.Database (connectToDb)
import qualified LearningJunkie.Web.API as Web
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Cors (myCors)
import LearningJunkie.Web.Environment (Env, Environment (Environment))
import LearningJunkie.Web.Minio (connectMinio)
import qualified LearningJunkie.Web.OpenApi as OpenApi
import Network.Minio (ConnectInfo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Auth.Server
import System.Environment (lookupEnv)

type LearningJunkieAPI = OpenApi.API :<|> Web.API

server :: ServerT LearningJunkieAPI AppM
server = OpenApi.server :<|> Web.server

api :: Proxy LearningJunkieAPI
api = Proxy

makeApp :: IO Application
makeApp = do
    onMissingFile (loadFile defaultConfig) (putStrLn "Missing environment variables")

    conns <- connectToDb

    myKey <- readKey "JWT-secret"

    serverName <- fromMaybe "http://localhost:3003/" <$> lookupEnv "SERVER_URL"

    bucketName <- fromMaybe "learning-junkie-aws-bucket" <$> lookupEnv "BUCKET_NAME"

    currentEnvString <- fromMaybe "Production" <$> lookupEnv "ENV"

    let
        currentEnv :: Env
        currentEnv = read currentEnvString

    minioConnection <- connectMinio currentEnv

    let
        jwtCfg = defaultJWTSettings myKey
        cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
        environment =
            Environment
                conns
                (Text.pack bucketName)
                minioConnection
                jwtCfg
                (Text.pack serverName)
                currentEnv

    return
        . myCors
        $ serveWithContext
            api
            cfg
        $ hoistServerWithContext
            api
            (Proxy :: Proxy '[CookieSettings, JWTSettings])
            (`runReaderT` environment)
            server

startApp :: IO ()
startApp = do
    app <- makeApp
    withStdoutLogger $ \aplogger -> do
        let settings =
                setPort 3003 $
                    setLogger aplogger defaultSettings

        runSettings settings app
