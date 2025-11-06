{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Certificates.Html.Certificate where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Time (UTCTime (utctDay))
import Data.UUID (toString)
import qualified LearningJunkie.CourseCompletions.CourseCompletion as CC
import qualified LearningJunkie.Courses.Course as Course
import qualified LearningJunkie.Universities.University as University
import qualified LearningJunkie.Users.User as User
import Lucid

certificateHtml :: Text.Text -> CC.CourseCompletion -> Html ()
certificateHtml serverUrl courseCompletion =
  let universityName = University.name . Course.university . CC.course $ courseCompletion
      universityLogo =
        fromMaybe
          "http://localhost:9000/learning-junkie-aws-bucket/logos/23463cbd-386f-4636-b32e-03601019481b.jpg"
          ( University.logo
              . Course.university
              . CC.course
              $ courseCompletion
          )
      universityAbbreviation =
        fromMaybe
          universityName
          (University.abbreviation . Course.university . CC.course $ courseCompletion)
      studentFullName = User.name . CC.user $ courseCompletion
      courseTitle = Course.title . CC.course $ courseCompletion
      instructorName = User.name . Course.instructor . CC.course $ courseCompletion
      completionDate = show . utctDay . CC.time $ courseCompletion
      certificateId = CC.id courseCompletion
      verificationUrl = serverUrl <> "certificates/" <> Text.pack (toString certificateId) <> "/verify"
   in doctypehtml_ $ do
        head_ $ do
          title_ $
            "Course Completion Certificate - " <> toHtml (toString certificateId)

          link_ [rel_ "stylesheet", href_ (serverUrl <> "static/certificates/style.css")]

          toHtmlRaw
            ( "<script src=\""
                <> "https://cdnjs.cloudflare.com/ajax/libs/qrcodejs/1.0.0/qrcode.min.js"
                <> "\"></script>" ::
                Text.Text
            )

          toHtmlRaw
            ( "<script src=\""
                <> serverUrl
                <> "static/certificates/qrcode.js"
                <> "\"></script>" ::
                Text.Text
            )

        body_ $ do
          div_ [id_ "certificate-body"] $ do
            div_ $ do
              div_ [id_ "certificate-header"] $ do
                span_ "Learning Junkie"
                div_ [id_ "university"] $ do
                  img_
                    [ id_ "university-logo"
                    , src_ universityLogo
                    , height_ "80px"
                    , width_ "80px"
                    ]
                  span_
                    (toHtml universityName)
            div_ [id_ "certificate-center"] $ do
              b_ $
                span_ "CERTIFICATE"
              span_
                ( "This is to certify that "
                    <> span_ (toHtml studentFullName)
                    <> " has successfully completed the course "
                    <> span_ [id_ "course-title"] (toHtml courseTitle)
                    <> " by completing the necessary number of lessons and exercises."
                )
              span_ [id_ "completion-date"] ("Completion Date: " <> toHtml completionDate)

            div_ [id_ "certificate-bottom"] $ do
              div_ [id_ "qrcode"] $ do
                span_ "Verification code"

              script_ ("generateVerificationQr(\"" <> verificationUrl <> "\")")

              div_ [id_ "signatures"] $ do
                div_ [class_ "signature"] $ do
                  span_ [class_ "name"] "Anvar Sheryatullaev"
                  span_ [class_ "affiliation"] "Learning Junkie Founder"
                div_ [class_ "signature"] $ do
                  span_ [class_ "name"] (toHtml instructorName)
                  span_ [class_ "affiliation"] ("Course Instructor, " <> toHtml universityAbbreviation)

            span_ [id_ "certificate-id"] $ "Certificate ID: " <> toHtml (toString certificateId)

{-
generateCertificate :: CC.CourseCompletion -> AppM (FilePath, ByteString)
generateCertificate courseCompletion = do
  let
    certificateFilePath :: FilePath
    certificateFilePath = "certificate-" <> toString (CC.id courseCompletion) <> ".html"
  return
    ( certificateFilePath
    , renderBS
        (certificateTemplate courseCompletion)
    )
-}
