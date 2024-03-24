{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Certificate (generateCertificateBS) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Diagrams.Backend.Rasterific (B, renderPdfBS)
import Diagrams.Prelude
import qualified Types.Course as C
import Types.Course.Completion (Completion)
import qualified Types.Course.Completion as CO
import qualified Types.University as UN
import qualified Types.User as US

text' :: Double -> String -> Diagram B
text' n t = text t # fontSizeL n # fc darkslategray <> strutY 0.2

rectWidth :: Double
rectWidth = 2

rectHeight :: Double
rectHeight = 1.414

certificateDiagram :: Completion -> Diagram B
certificateDiagram completion =
    ( ( ( (text' 0.04 "LEARNING JUNKIE" <> strutX 1)
            ||| (text' 0.04 (T.unpack . UN.name . C.university . CO.course $ completion) <> strutX 1)
        )
            # center
      )
        === text' 0.07 "Certificate of Completion"
        === text' 0.12 (T.unpack . US.name . CO.user $ completion)
        # bold
        === text' 0.05 "Has successfully completed the course"
        === text (T.unpack . C.title . CO.course $ completion)
        # fontSizeL 0.05
        # fc darkslategray
        === strutY 0.2
        === text'
            0.04
            ( "Course Instructor: "
                <> (T.unpack . US.name . C.instructor . CO.course $ completion)
                <> ", "
                <> universityLabel
            )
        === text ("Time of completion: " <> (show . CO.time $ completion))
        # fontSizeL 0.02
    )
        # center
        <> rect rectWidth rectHeight # lw none # fc whitesmoke # center
  where
    universityLabel = case UN.abbreviation . C.university . CO.course $ completion of
        Nothing -> T.unpack . US.name . C.instructor . CO.course $ completion
        Just abbr -> T.unpack abbr

generateCertificateBS :: Completion -> ByteString
generateCertificateBS = renderPdfBS 2000 1414 (dims2D 2000 1414) . certificateDiagram
