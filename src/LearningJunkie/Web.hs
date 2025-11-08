{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Web (startApp) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import qualified Data.Text as Text
import LearningJunkie.Certificates.EmbedStatic (initializeStaticFiles)
import LearningJunkie.Database (connectToDb)
import qualified LearningJunkie.Web.API as Web
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Cors (myCors)
import LearningJunkie.Web.Environment (Environment (Environment, jwtSettings), HaskellEnv (Development, Production))
import LearningJunkie.Web.Minio (connectMinio)
import qualified LearningJunkie.Web.OpenApi as OpenApi
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

initializeEnvironment :: MaybeT IO Environment
initializeEnvironment = do
    liftIO initializeStaticFiles

    eitherLoaded <- liftIO (try (loadFile defaultConfig) :: IO (Either IOException ()))

    case eitherLoaded of
        Left _ -> do
            liftIO $ putStrLn "Dotenv file not found, loading supplied variables"

            loadVariables
        Right _ -> do
            liftIO $ putStrLn "Found Dotenv. Loading the file..."

            loadVariables
  where
    loadVariables = do
        haskellEnvString <- hoistMaybe =<< liftIO (lookupEnv "HASKELL_ENV")

        haskellEnv <- liftIO (readIO haskellEnvString :: IO HaskellEnv)

        connectString <- hoistMaybe =<< liftIO (lookupEnv "DATABASE_URL")

        conns <- liftIO $ connectToDb connectString

        bucketName <- Text.pack <$> (hoistMaybe =<< liftIO (lookupEnv "BUCKET_NAME"))

        minioConn <- liftIO $ connectMinio haskellEnv

        jwts <-
            liftIO $
                defaultJWTSettings
                    <$> readKey
                        ( case haskellEnv of
                            Development -> "JWT-secret"
                            Production -> "/etc/secrets/JWT-secret"
                        )

        serverUrl <- Text.pack <$> (hoistMaybe =<< liftIO (lookupEnv "SERVER_URL"))

        return $
            Environment
                haskellEnv
                conns
                bucketName
                minioConn
                jwts
                serverUrl

makeApp :: MaybeT IO Application
makeApp = do
    environment <- initializeEnvironment

    let
        jwtCfg = jwtSettings environment
        cfg = defaultCookieSettings :. jwtCfg :. EmptyContext

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
    mbApp <- runMaybeT makeApp

    case mbApp of
        Nothing -> putStrLn "Could not initialize environment. Perhaps some environment variables are missing?"
        Just app ->
            withStdoutLogger $ \aplogger -> do
                let settings =
                        setPort 3003 $
                            setLogger aplogger defaultSettings

                runSettings settings app
