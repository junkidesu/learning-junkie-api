{-# LANGUAGE OverloadedStrings #-}

module Lib (
    startApp,
) where

import Api (api, server)
import Database (initializeConnectionPool)
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

    let
        jwts = defaultJWTSettings jwk
        app =
            myCors $
                serveWithContext
                    api
                    (defaultCookieSettings :. jwts :. EmptyContext)
                    (server conns jwts)

    withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings app
