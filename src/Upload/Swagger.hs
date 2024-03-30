{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Upload.Swagger where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Multipart (MultipartData, MultipartForm)
import Servant.Swagger
import Servant.Swagger.Internal

instance (HasSwagger api) => HasSwagger (MultipartForm tag (MultipartData tag) :> api) where
    toSwagger :: Proxy (MultipartForm tag (MultipartData tag) :> api) -> Swagger
    toSwagger Proxy =
        toSwagger (Proxy :: Proxy api)
            & addParam param
      where
        param =
            mempty
                & name
                    .~ "file"
                & required
                    ?~ True
                & description
                    ?~ "File to upload"
                & schema
                    .~ ParamOther
                        ( mempty
                            & in_
                                .~ ParamFormData
                            & paramSchema
                                .~ (mempty & type_ ?~ SwaggerFile)
                        )
