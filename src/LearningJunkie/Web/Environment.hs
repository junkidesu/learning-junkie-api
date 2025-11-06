module LearningJunkie.Web.Environment where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.Beam.Postgres (Connection)
import Network.Minio (MinioConn)
import Servant.Auth.Server (JWTSettings)

data Environment = Environment
    { dbConnection :: Pool Connection
    , minioBucket :: Text
    , minioConnection :: MinioConn
    , jwtSettings :: JWTSettings
    , serverUrl :: Text
    }
