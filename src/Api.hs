{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api (API, api, server) where

import Api.Users (UsersAPI, usersServer)
import Control.Lens
import Data.Pool (Pool)
import Data.Swagger
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Swagger
import Servant.Swagger.UI

type API' = UsersAPI

usersOpts :: Traversal' Swagger Operation
usersOpts = subOperations (Proxy :: Proxy UsersAPI) (Proxy :: Proxy API')

type API = API' :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc =
    toSwagger (Proxy :: Proxy API')
        & info . title .~ "Learning Junkie"
        & info . version .~ "0.1.0.0"
        & info . description ?~ "Free Educational Platform"
        & info . license ?~ "BSD"
        & applyTagsFor usersOpts ["users" & description ?~ "Operations on users"]

api :: Proxy API
api = Proxy

server' :: Pool Connection -> Server API'
server' conns = usersServer conns

server :: Pool Connection -> Server API
server conns = server' conns :<|> swaggerSchemaUIServer swaggerDoc
