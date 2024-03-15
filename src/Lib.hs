module Lib (
    startApp,
) where

import Api (api, server)
import Database (initializeConnectionPool)
import Network.Wai.Handler.Warp (
    defaultSettings,
    runSettings,
    setLogger,
    setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import Servant

port :: Int
port = 3001

startApp :: IO ()
startApp = do
    conns <- initializeConnectionPool

    let app = serveWithContext api EmptyContext (server conns)

    withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger $ defaultSettings
        runSettings settings app
