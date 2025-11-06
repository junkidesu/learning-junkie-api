{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Certificates.Web.Verify where

import Data.UUID (UUID)
import LearningJunkie.Certificates.Html.Verification (verificationHtml)
import LearningJunkie.CourseCompletions.Database (selectCourseCompletionByCertificateId, toCourseCompletionType)
import LearningJunkie.Web.AppM (AppM)
import Lucid (Html)
import Servant
import Servant.HTML.Lucid (HTML)

type API =
  Summary "The endpoint for verifying the certificate by ID"
    :> Capture' [Required, Description "ID of the generated certificate"] "id" UUID
    :> "verify"
    :> Get '[HTML] (Html ())

handler :: UUID -> AppM (Html ())
handler certificateId = do
  mbCourseCompletion <- selectCourseCompletionByCertificateId certificateId

  case mbCourseCompletion of
    Nothing -> throwError err404
    Just courseCompletion -> do
      pure $ verificationHtml . toCourseCompletionType $ courseCompletion
