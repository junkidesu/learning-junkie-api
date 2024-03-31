module Database.Operations.Courses.Banner (setBanner, deleteCourseBanner) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database (updateReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Courses.Banner (deleteCourseBannerQ, setBannerQ)
import Types.Course (Course)

setBanner :: Pool Connection -> Int -> Text -> IO (Maybe Course)
setBanner conns courseId bannerUrl = updateReturning conns setBannerQ (bannerUrl, courseId)

deleteCourseBanner :: Pool Connection -> Int -> IO (Maybe Course)
deleteCourseBanner conns courseId = updateReturning conns deleteCourseBannerQ (Only courseId)
