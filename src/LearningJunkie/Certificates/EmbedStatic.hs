{-# LANGUAGE TemplateHaskell #-}

module LearningJunkie.Certificates.EmbedStatic where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)

staticDir :: [(FilePath, BS.ByteString)]
staticDir = $(embedDir "static/certificates")

initializeStaticFiles :: IO ()
initializeStaticFiles = do
  let staticDirectory = "static/certificates"

  createDirectoryIfMissing True staticDirectory

  forM_ staticDir $ \(filepath, file) -> do
    fileExists <- doesFileExist filepath

    unless fileExists $
      BS.writeFile (staticDirectory <> "/" <> filepath) file
