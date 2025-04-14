module LearningJunkie.Web.Environment where

import Data.Pool (Pool)
import Database.Beam.Postgres (Connection)
import Servant.Auth.Server (JWTSettings)

data Environment = Environment
    { dbConnection :: Pool Connection
    , jwtSettings :: JWTSettings
    }
