{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Auth.OpenApi () where

import Control.Lens
import qualified Data.HashMap.Strict.InsOrd as HM
import Data.OpenApi
import Data.Proxy
import qualified Data.Text as T
import Servant.API
import qualified Servant.Auth
import Servant.OpenApi

instance (HasOpenApi api) => HasOpenApi (Servant.Auth.Auth '[] a :> api) where
        toOpenApi Proxy = toOpenApi $ Proxy @api

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.BasicAuth : auths) a :> api) where
        toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
            where
                addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
                identifier :: T.Text = "BasicAuth"
                securityScheme =
                        SecurityScheme
                                { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
                                , _securitySchemeDescription = Just "Basic Authentication"
                                }

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.JWT : auths) a :> api) where
        toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
            where
                addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
                identifier :: T.Text = "JWT"
                securityScheme =
                        SecurityScheme
                                { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
                                , _securitySchemeDescription = Just "Bearer Authentication"
                                }

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.Cookie : auths) a :> api) where
        toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
            where
                addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
                identifier :: T.Text = "Cookie"
                securityScheme =
                        SecurityScheme
                                { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
                                , _securitySchemeDescription = Just "Cookie Authentication"
                                }

addSecurityScheme :: T.Text -> SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
        openApi
                { _openApiComponents =
                        (_openApiComponents openApi)
                                { _componentsSecuritySchemes =
                                        _componentsSecuritySchemes (_openApiComponents openApi)
                                                <> SecurityDefinitions (HM.singleton securityIdentifier securityScheme)
                                }
                }

addSecurityRequirement :: T.Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
        allOperations
                . security
                %~ ((SecurityRequirement $ HM.singleton securityRequirement []) :)
