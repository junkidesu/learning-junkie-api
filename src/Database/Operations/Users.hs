module Database.Operations.Users (
        allUsers,
        insertUser,
        userById,
        userByEmail,
) where

import Data.Password.Bcrypt
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.Queries.Users
import Types.User
import qualified Types.User.NewUser as NU

allUsers :: Pool Connection -> IO [User]
allUsers conns = withResource conns $
        \conn -> query_ conn allUsersQ

insertUser :: Pool Connection -> NU.NewUser -> IO User
insertUser conns newUser = withResource conns $
        \conn -> do
                hashedPassword <- hashPassword . mkPassword . NU.password $ newUser
                (user : _) <-
                        query
                                conn
                                insertUserQ
                                ( NU.name newUser
                                , NU.birthday newUser
                                , NU.education newUser
                                , NU.role newUser
                                , NU.email newUser
                                , unPasswordHash hashedPassword
                                )
                return user
userById :: Pool Connection -> Int -> IO (Maybe User)
userById conns userId = do
        withResource conns $
                \conn -> do
                        found <- query conn userByIdQ (Only userId)
                        case found of
                                [] -> pure Nothing
                                (user : _) -> pure . Just $ user

userByEmail :: Pool Connection -> Text -> IO (Maybe User)
userByEmail conns e = withResource conns $
        \conn -> do
                found <- query conn userByEmailQ (Only e)

                case found of
                        [] -> pure Nothing
                        (user : _) -> pure . Just $ user
