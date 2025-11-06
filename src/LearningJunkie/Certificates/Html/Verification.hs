{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Certificates.Html.Verification where

import Data.Maybe (fromMaybe)
import Data.Time (UTCTime (utctDay))
import Data.UUID (toString)
import qualified LearningJunkie.CourseCompletions.CourseCompletion as CC
import qualified LearningJunkie.Courses.Course as Course
import qualified LearningJunkie.Universities.University as University
import qualified LearningJunkie.Users.User as User
import Lucid

verificationHtml :: CC.CourseCompletion -> Html ()
verificationHtml courseCompletion =
  let universityName = University.name . Course.university . CC.course $ courseCompletion
      universityAbbreviation =
        fromMaybe
          universityName
          (University.abbreviation . Course.university . CC.course $ courseCompletion)
      studentFullName = User.name . CC.user $ courseCompletion
      courseTitle = Course.title . CC.course $ courseCompletion
      instructorName = User.name . Course.instructor . CC.course $ courseCompletion
      completionDate = show . utctDay . CC.time $ courseCompletion
      certificateId = CC.id courseCompletion
   in doctypehtml_ $ do
        head_ $ do
          title_ "Certificate Verification - "
        body_ $ do
          h3_ "Learning Junkie - Certificate Verification"

          p_ "This page serves to verify the validity of the course completion certificate. You must have ended up on this page by scanning the QR code on the certificate."

          hr_ []

          p_ $ "Certificate id: " <> toHtml (toString certificateId)

          p_ $ "Student's full name: " <> toHtml studentFullName

          p_ $ "Course title: " <> toHtml courseTitle

          p_ $ "University: " <> toHtml universityName <> ", " <> toHtml universityAbbreviation

          p_ $ "Instructor's full name: " <> toHtml instructorName

          p_ $ "Completion date: " <> toHtml completionDate
