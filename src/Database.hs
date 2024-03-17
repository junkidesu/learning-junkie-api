module Database (
    initializeConnectionPool,
    toSqlQuery,
    getMany_,
    getMany,
    insert,
    insertReturning,
    getOne,
    delete,
) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.UTF8 (fromString)
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
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

getMany_ :: (FromRow a) => Pool Connection -> Query -> IO [a]
getMany_ conns q = withResource conns $
    \conn -> query_ conn q

getMany :: (FromRow a, ToRow b) => Pool Connection -> Query -> b -> IO [a]
getMany conns q args = withResource conns $
    \conn -> query conn q args

insert :: (ToRow b) => Pool Connection -> Query -> b -> IO ()
insert conns q args = withResource conns $
    \conn -> void $ execute conn q args

insertReturning :: (FromRow a, ToRow b) => Pool Connection -> Query -> b -> IO a
insertReturning conns q args = withResource conns $
    \conn -> do
        [inserted] <- query conn q args

        return inserted

getOne :: (FromRow a, ToRow b) => Pool Connection -> Query -> b -> IO (Maybe a)
getOne conns q args =
    withResource conns $
        \conn -> do
            result <- query conn q args

            case result of
                [] -> return Nothing
                (row : _) -> return . pure $ row

delete :: (ToRow a) => Pool Connection -> Query -> a -> IO ()
delete conns q args =
    withResource conns $
        \conn ->
            void $ execute conn q args
