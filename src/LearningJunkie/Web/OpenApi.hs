{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Web.OpenApi (API, server) where

import Control.Lens
import Data.OpenApi hiding (server)
import qualified LearningJunkie.Courses.Web as Courses
import qualified LearningJunkie.Universities.Web as Universities
import qualified LearningJunkie.Users.Web as Users
import qualified LearningJunkie.Web.API as Web
import LearningJunkie.Web.AppM (AppM)
import Servant (HasServer (ServerT), Proxy (Proxy))
import Servant.OpenApi
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServerT)

type API = SwaggerSchemaUI "swagger-ui" "swagger.json"

universitiesOps :: Traversal' OpenApi Operation
universitiesOps = subOperations (Proxy :: Proxy Universities.API) (Proxy :: Proxy Web.API)

usersOps :: Traversal' OpenApi Operation
usersOps = subOperations (Proxy :: Proxy Users.API) (Proxy :: Proxy Web.API)

coursesOps :: Traversal' OpenApi Operation
coursesOps = subOperations (Proxy :: Proxy Courses.API) (Proxy :: Proxy Web.API)

openApiDoc :: OpenApi
openApiDoc =
    toOpenApi (Proxy :: Proxy Web.API)
        & info
            . title
            .~ "Learning Junkie API"
        & info
            . version
            .~ "0.1.0.0"
        & info
            . description
            ?~ "Free Online Education Platform"
        & info
            . license
            ?~ "BSD"
        & applyTagsFor universitiesOps ["universities" & description ?~ "Operations on universities"]
        & applyTagsFor usersOps ["users" & description ?~ "Operations on users"]
        & applyTagsFor coursesOps ["courses" & description ?~ "Operations on courses"]

server :: ServerT API AppM
server = swaggerSchemaUIServerT openApiDoc
