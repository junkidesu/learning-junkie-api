module Database.Operations.Users.Avatar (setAvatar, deleteUserAvatar) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database (updateReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Users.Avatar (deleteUserAvatarQ, setAvatarQ)
import Types.User

setAvatar :: Pool Connection -> Int -> Text -> IO (Maybe User)
setAvatar conns userId avatarUrl = updateReturning conns setAvatarQ (avatarUrl, userId)

deleteUserAvatar :: Pool Connection -> Int -> IO (Maybe User)
deleteUserAvatar conns userId = updateReturning conns deleteUserAvatarQ (Only userId)
