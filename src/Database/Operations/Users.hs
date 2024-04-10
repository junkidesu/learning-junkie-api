module Database.Operations.Users (
        allUsers,
        insertUser,
        userById,
        userByEmail,
        deleteUser,
) where

import Data.Password.Bcrypt
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Database
import Database.PostgreSQL.Simple
import Database.Queries.Users
import Types.User
import qualified Types.User.NewUser as NU

allUsers :: Pool Connection -> IO [User]
allUsers conns = getMany_ conns allUsersQ

insertUser :: Pool Connection -> NU.NewUser -> IO User
insertUser conns newUser = do
        hashedPassword <- hashPassword . mkPassword . T.strip . NU.password $ newUser
        insertReturning
                conns
                insertUserQ
                ( T.strip . NU.name $ newUser
                , NU.birthday newUser
                , NU.education newUser
                , T.strip . NU.email $ newUser
                , unPasswordHash hashedPassword
                )

userById :: Pool Connection -> Int -> IO (Maybe User)
userById conns userId = getOne conns userByIdQ (Only userId)

userByEmail :: Pool Connection -> Text -> IO (Maybe User)
userByEmail conns e = getOne conns userByEmailQ (Only e)

deleteUser :: Pool Connection -> Int -> IO ()
deleteUser conns userId = delete conns deleteUserQ (Only userId)
