{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Database where

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.Beam
import Database.Beam.Postgres
import LearningJunkie.Courses.Database.Table (CourseT)
import LearningJunkie.Universities.Database.Table (UniversityT)
import LearningJunkie.Users.Database.Table (UserT (_userPasswordHash))
import System.Environment (getEnv)

data LearningJunkieDb f = LearningJunkieDb
    { dbUniversities :: f (TableEntity UniversityT)
    , dbUsers :: f (TableEntity UserT)
    , dbCourses :: f (TableEntity CourseT)
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
            , dbCourses = setEntityName "courses"
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
