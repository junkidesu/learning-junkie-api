module Database.Operations.Universities.Logo (
        setLogo,
        deleteUniversityLogo,
)
where

import Data.Pool (Pool)
import Data.Text (Text)
import Database (updateReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Universities.Logo (deleteUniversityLogoQ, setLogoQ)
import Types.University (University)

setLogo :: Pool Connection -> Int -> Text -> IO (Maybe University)
setLogo conns universityId logoUrl = updateReturning conns setLogoQ (logoUrl, universityId)

deleteUniversityLogo :: Pool Connection -> Int -> IO (Maybe University)
deleteUniversityLogo conns universityId = updateReturning conns deleteUniversityLogoQ (Only universityId)
