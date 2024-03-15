module Database (initializeConnectionPool) where

import Data.ByteString.UTF8 (fromString)
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes)
import Database.PostgreSQL.Simple
import System.Environment (getEnv)

initializeConnectionPool :: IO (Pool Connection)
initializeConnectionPool = do
    dbConnString <- fromString <$> getEnv "DATABASE_URL"
    newPool
        . setNumStripes
            (Just 2)
        $ defaultPoolConfig
            (connectPostgreSQL dbConnString)
            close
            60
            10
