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
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (JWTSettings)
import Servant.Auth.Swagger ()
import Servant.Swagger
import Servant.Swagger.UI
import Upload.Environment (S3Environment)
import Upload.Swagger ()

type Ping = Summary "Utility endpoint for health check" :> "ping" :> Get '[PlainText] T.Text

type API' =
    Ping
        :<|> AuthAPI
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

utilitiesOpts :: Traversal' Swagger Operation
utilitiesOpts = subOperations (Proxy :: Proxy Ping) (Proxy :: Proxy API')

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
        & applyTagsFor utilitiesOpts ["utilities" & description ?~ "Utility operations"]

api :: Proxy API
api = Proxy

server' :: Pool Connection -> S3Environment -> JWTSettings -> Server API'
server' conns s3env jwts =
    ping
        :<|> authServer conns jwts
        :<|> usersServer conns s3env
        :<|> universitiesServer conns s3env
        :<|> coursesServer conns s3env
        :<|> exercisesServer conns
  where
    ping :: Handler T.Text
    ping = return "pong"

server :: Pool Connection -> S3Environment -> JWTSettings -> Server API
server conns s3env jwts = server' conns s3env jwts :<|> swaggerSchemaUIServer swaggerDoc
