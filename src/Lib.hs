{-# LANGUAGE OverloadedStrings #-}

module Lib (
    startApp,
) where

import Api (api, server)
import Aws (baseConfiguration)
import Aws.Core (Protocol (HTTPS))
import Aws.S3 (S3SignPayloadMode (SignWithEffort), s3v4)
import Database (initializeConnectionPool)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp (
    defaultSettings,
    runSettings,
    setLogger,
    setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
import Servant
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, generateKey)

port :: Int
port = 3001

myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy =
        simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type"]
            , corsMethods = "PUT" : simpleMethods
            }

startApp :: IO ()
startApp = do
    conns <- initializeConnectionPool

    jwk <- generateKey

    cfg <- baseConfiguration
    mgr <- newManager tlsManagerSettings

    let
        jwts = defaultJWTSettings jwk
        s3cfg = s3v4 HTTPS "s3.us-east-1.amazonaws.com" False SignWithEffort
        app =
            myCors $
                serveWithContext
                    api
                    (defaultCookieSettings :. jwts :. EmptyContext)
                    (server conns cfg s3cfg mgr jwts)

    withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings app
