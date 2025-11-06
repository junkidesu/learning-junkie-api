{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Web.OpenApi (API, server) where

import Control.Lens
import Data.OpenApi hiding (server)
import qualified LearningJunkie.Certificates.Web as Certificates
import qualified LearningJunkie.Codex.Web as Codex
import qualified LearningJunkie.Courses.Web as Courses
import qualified LearningJunkie.Exercises.Web as Exercises
import qualified LearningJunkie.Lessons.Web as Lessons
import qualified LearningJunkie.Submissions.Web as Submissions
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

lessonsOps :: Traversal' OpenApi Operation
lessonsOps = subOperations (Proxy :: Proxy Lessons.API) (Proxy :: Proxy Web.API)

exercisesOps :: Traversal' OpenApi Operation
exercisesOps = subOperations (Proxy :: Proxy Exercises.API) (Proxy :: Proxy Web.API)

submissionsOps :: Traversal' OpenApi Operation
submissionsOps = subOperations (Proxy :: Proxy Submissions.API) (Proxy :: Proxy Web.API)

codexOps :: Traversal' OpenApi Operation
codexOps = subOperations (Proxy :: Proxy Codex.API) (Proxy :: Proxy Web.API)

certificatesOps :: Traversal' OpenApi Operation
certificatesOps = subOperations (Proxy :: Proxy Certificates.API) (Proxy :: Proxy Web.API)

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
        & applyTagsFor lessonsOps ["lessons" & description ?~ "Operations on lessons"]
        & applyTagsFor exercisesOps ["exercises" & description ?~ "Operations on exercises"]
        & applyTagsFor submissionsOps ["submissions" & description ?~ "Operations on submissions"]
        & applyTagsFor codexOps ["codex" & description ?~ "Code execution utility"]
        & applyTagsFor certificatesOps ["certificates" & description ?~ "Operations on certificates"]

server :: ServerT API AppM
server = swaggerSchemaUIServerT openApiDoc
