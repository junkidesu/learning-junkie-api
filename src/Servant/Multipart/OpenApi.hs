{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Multipart.OpenApi () where

import Control.Lens
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.OpenApi
import Network.HTTP.Media
import Servant
import Servant.Multipart (Mem, MultipartData, MultipartForm)
import Servant.OpenApi
import Servant.OpenApi.Internal

instance (HasOpenApi api) => HasOpenApi (MultipartForm Mem (MultipartData Mem) :> api) where
    toOpenApi :: Proxy (MultipartForm Mem (MultipartData Mem) :> api) -> OpenApi
    toOpenApi _ = toOpenApi (Proxy :: Proxy api) & addRequestBody body
      where
        myMediaType = BSU.fromString "multipart" // BSU.fromString "form-data"
        myProperties =
            [
                ( "file"
                , Inline
                    ( mempty
                        & type_ ?~ OpenApiString
                        & format ?~ "binary"
                        & required .~ ["file"]
                    )
                )
            ]
        mySchema =
            Inline $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties .~ myProperties
        body =
            (mempty :: RequestBody)
                & content
                    .~ InsOrdHashMap.fromList
                        [(myMediaType, mempty & schema ?~ mySchema)]
                & required ?~ True
