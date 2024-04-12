{-# LANGUAGE OverloadedStrings #-}

module Lib (
    startApp,
) where

import Api (api, server)
import Aws (baseConfiguration)
import Aws.Core (Protocol (HTTPS))
import Aws.S3 (S3SignPayloadMode (SignWithEffort), s3v4)
import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Cors (myCors)
import Database (initializeConnectionPool)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.Wai.Handler.Warp (
    defaultSettings,
    runSettings,
    setLogger,
    setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, generateKey)
import System.Environment (lookupEnv)
import Upload.Environment (S3Environment (S3Environment))

startApp :: IO ()
startApp = do
    onMissingFile (loadFile defaultConfig) (pure ())

    mbPort <- lookupEnv "PORT"

    let port :: Int
        port = maybe 3001 read mbPort

    conns <- initializeConnectionPool

    jwk <- generateKey

    cfg <- baseConfiguration
    mgr <- newManager tlsManagerSettings

    let
        jwts = defaultJWTSettings jwk
        s3cfg = s3v4 HTTPS "s3.eu-north-1.amazonaws.com" False SignWithEffort
        app =
            myCors $
                serveWithContext
                    api
                    (defaultCookieSettings :. jwts :. EmptyContext)
                    (server conns (S3Environment cfg s3cfg mgr) jwts)

    withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings app
