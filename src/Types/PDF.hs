{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.PDF (PDF (..)) where

import Data.ByteString.Lazy (ByteString)
import Data.Swagger (NamedSchema (NamedSchema), ToSchema (declareNamedSchema), binarySchema)
import Servant (MimeRender (mimeRender), OctetStream)

newtype PDF = PDF ByteString

instance MimeRender OctetStream PDF where
    mimeRender proxy (PDF bs) = mimeRender proxy bs

instance ToSchema PDF where
    declareNamedSchema _ = return $ NamedSchema (Just "PDF") binarySchema
