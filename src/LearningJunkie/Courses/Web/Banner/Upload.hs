{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Banner.Upload where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import LearningJunkie.Courses.Course (Course)
import qualified LearningJunkie.Courses.Course.Attributes as Attributes
import LearningJunkie.Courses.Database (toCourseType, updateCourse)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import LearningJunkie.Web.Minio (uploadFileMinio)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Servant.Multipart
import Servant.Multipart.OpenApi ()

type API =
    Summary "Upload new course banner"
        :> JWTAuth
        :> MultipartForm Mem (MultipartData Mem)
        :> Post '[JSON] Course

handler :: Int32 -> AuthResult Auth.AuthUser -> MultipartData Mem -> AppM Course
handler courseId (Authenticated authUser) multipartData = Permissions.requirePermissionWithId
    authUser
    ( Permissions.Permission
        Permissions.SameInstructor
        Permissions.Update
        Permissions.Course
    )
    (Just courseId)
    $ do
        let eitherFileData = lookupFile "file" multipartData

        case eitherFileData of
            Left _ -> throwError err400{errBody = "Multipart body not attached"}
            Right fileData -> do
                let
                    bannerOriginalFileName = fdFileName fileData

                    bannerContentType = fdFileCType fileData

                    extension = last $ T.splitOn "." bannerOriginalFileName

                    payload :: BS.ByteString
                    payload = BS.toStrict $ fdPayload fileData

                bannerFileNameId <- toText <$> liftIO nextRandom

                let logoFilePath = "banners/" <> bannerFileNameId <> "." <> extension

                logoUploadResult <- uploadFileMinio logoFilePath bannerContentType payload

                case logoUploadResult of
                    Left e -> do
                        liftIO $ print e
                        throwError err400{errBody = "File not uploaded"}
                    Right objectUrl -> do
                        university <-
                            updateCourse
                                courseId
                                Attributes.emptyEditCourse{Attributes.banner = Just (Just objectUrl)}

                        return $ toCourseType university
handler _ _ _ = throwError err401
