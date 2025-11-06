{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.HTML.Lucid.OpenApi (ToSchema (..)) where

import Data.OpenApi
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import Lucid (Html)

instance ToSchema (Html ()) where
        declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text.Text)
