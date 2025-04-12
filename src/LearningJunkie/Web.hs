{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Web (startApp) where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import LearningJunkie.Database (connectToDb)
import qualified LearningJunkie.Web.API as Web
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Environment (Environment (Environment))
import qualified LearningJunkie.Web.OpenApi as OpenApi
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant

type LearningJunkieAPI = OpenApi.API :<|> Web.API

server :: ServerT LearningJunkieAPI AppM
server = OpenApi.server :<|> Web.server

api :: Proxy LearningJunkieAPI
api = Proxy

makeApp :: IO Application
makeApp = do
    onMissingFile (loadFile defaultConfig) (putStrLn "Missing environment variables")

    conns <- connectToDb

    let environment = Environment conns
        cfg = EmptyContext
    return
        . serveWithContext
            api
            cfg
        $ hoistServerWithContext
            api
            (Proxy :: Proxy '[])
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
