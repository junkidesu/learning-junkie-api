{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Database where

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes)
import Database.Beam (Database, DatabaseSettings, Generic, TableEntity, dbModification, defaultDbSettings, setEntityName, withDbModification)
import Database.Beam.Postgres (Connection, close, connectPostgreSQL)
import LearningJunkie.Universities.Database.Table (UniversityT)
import System.Environment (getEnv)

data LearningJunkieDb f = LearningJunkieDb
    { universities :: f (TableEntity UniversityT)
    }
    deriving (Generic, Database be)

db :: DatabaseSettings db LearningJunkieDb
db =
    defaultDbSettings
        `withDbModification` dbModification
            { universities = setEntityName "universities"
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
