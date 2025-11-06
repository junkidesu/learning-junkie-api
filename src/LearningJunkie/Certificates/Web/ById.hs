{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Certificates.Web.ById where

import Control.Monad.Trans.Reader (asks)
import Data.UUID (UUID)
import LearningJunkie.Certificates.Html.Certificate (certificateHtml)
import LearningJunkie.CourseCompletions.Database (
  selectCourseCompletionByCertificateId,
  toCourseCompletionType,
 )
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Environment (Environment (serverUrl))
import Lucid (Html)
import Servant
import Servant.HTML.Lucid
import Servant.HTML.Lucid.OpenApi ()

type API =
  Summary "Query certificate by the certificate ID"
    :> Capture' [Required, Description "ID of the generated certificate"] "id" UUID
    :> Get '[HTML] (Html ())

handler :: UUID -> AppM (Html ())
handler certificateId = do
  mbCourseCompletion <- selectCourseCompletionByCertificateId certificateId

  url <- asks serverUrl

  case mbCourseCompletion of
    Nothing -> throwError err404
    Just courseCompletion -> do
      pure $ certificateHtml url . toCourseCompletionType $ courseCompletion
