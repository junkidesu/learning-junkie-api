{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api (API, api, server) where

import Api.Auth (AuthAPI, authServer)
import Api.Courses (CoursesAPI, coursesServer)
import Api.Exercises (ExercisesAPI, exercisesServer)
import Api.Universities (UniversitiesAPI, universitiesServer)
import Api.Users (UsersAPI, usersServer)
import Control.Lens
import Data.Pool (Pool)
import Data.Swagger
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (JWTSettings)
import Servant.Auth.Swagger ()
import Servant.Swagger
import Servant.Swagger.UI

type API' =
    AuthAPI
        :<|> UsersAPI
        :<|> UniversitiesAPI
        :<|> CoursesAPI
        :<|> ExercisesAPI

authOpts :: Traversal' Swagger Operation
authOpts = subOperations (Proxy :: Proxy AuthAPI) (Proxy :: Proxy API')

usersOpts :: Traversal' Swagger Operation
usersOpts = subOperations (Proxy :: Proxy UsersAPI) (Proxy :: Proxy API')

universitiesOpts :: Traversal' Swagger Operation
universitiesOpts = subOperations (Proxy :: Proxy UniversitiesAPI) (Proxy :: Proxy API')

coursesOpts :: Traversal' Swagger Operation
coursesOpts = subOperations (Proxy :: Proxy CoursesAPI) (Proxy :: Proxy API')

exercisesOpts :: Traversal' Swagger Operation
exercisesOpts = subOperations (Proxy :: Proxy ExercisesAPI) (Proxy :: Proxy API')

type API = API' :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc =
    toSwagger (Proxy :: Proxy API')
        & info . title .~ "Learning Junkie API"
        & info . version .~ "0.1.0.0"
        & info . description ?~ "Free Online Education Platform"
        & info . license ?~ "BSD"
        & applyTagsFor authOpts ["authentication" & description ?~ "Authenticating to the site"]
        & applyTagsFor usersOpts ["users" & description ?~ "Operations on users"]
        & applyTagsFor universitiesOpts ["universities" & description ?~ "Operations on universities"]
        & applyTagsFor coursesOpts ["courses" & description ?~ "Operations on courses"]
        & applyTagsFor exercisesOpts ["exercises" & description ?~ "Operations on exercises"]

api :: Proxy API
api = Proxy

server' :: Pool Connection -> JWTSettings -> Server API'
server' conns jwts =
    authServer conns jwts
        :<|> usersServer conns
        :<|> universitiesServer conns
        :<|> coursesServer conns
        :<|> exercisesServer conns

server :: Pool Connection -> JWTSettings -> Server API
server conns jwts = server' conns jwts :<|> swaggerSchemaUIServer swaggerDoc
