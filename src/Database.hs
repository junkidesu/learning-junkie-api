module Database (initializeConnectionPool, toSqlQuery) where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.UTF8 (fromString)
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (Query))
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

toSqlQuery :: [BS.ByteString] -> Query
toSqlQuery = Query . BS.unlines
