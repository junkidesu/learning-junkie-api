{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Database where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.Beam (Database, DatabaseSettings, Generic, MonadIO (liftIO), TableEntity, dbModification, defaultDbSettings, fieldNamed, modifyTableFields, setEntityName, tableModification, withDbModification)
import Database.Beam.Postgres (Connection, close, connectPostgreSQL)
import LearningJunkie.Universities.Database.Table (UniversityT)
import LearningJunkie.Users.Database.Table (UserT (_userPasswordHash, _userUniversity))
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Environment (Environment (dbConnection))
import System.Environment (getEnv)

data LearningJunkieDb f = LearningJunkieDb
    { dbUniversities :: f (TableEntity UniversityT)
    , dbUsers :: f (TableEntity UserT)
    }
    deriving (Generic, Database be)

db :: DatabaseSettings db LearningJunkieDb
db =
    defaultDbSettings
        `withDbModification` dbModification
            { dbUniversities = setEntityName "universities"
            , dbUsers =
                setEntityName "users"
                    <> modifyTableFields
                        tableModification
                            { _userPasswordHash = fieldNamed "passwordhash"
                            }
            }

connectToDb :: IO (Pool Connection)
connectToDb = do
    loadFile defaultConfig
    connectString <- getEnv "DATABASE_URL"

    newPool . setNumStripes (Just 2) $
        defaultPoolConfig
            (connectPostgreSQL $ BSU.fromString connectString)
            close
            60
            10

withConnection :: (Connection -> IO a) -> AppM a
withConnection op = do
    conns <- asks dbConnection

    liftIO $ withResource conns $ \conn ->
        op conn
