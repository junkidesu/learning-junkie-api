module Database.Operations.Users.Avatar (setAvatar) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database (updateReturning)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Users.Avatar (setAvatarQ)
import Types.User

setAvatar :: Pool Connection -> Int -> Text -> IO (Maybe User)
setAvatar conns userId avatarUrl = updateReturning conns setAvatarQ (avatarUrl, userId)
