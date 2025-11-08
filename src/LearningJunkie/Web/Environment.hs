module LearningJunkie.Web.Environment where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.Beam.Postgres (Connection)
import Network.Minio (MinioConn)
import Servant.Auth.Server (JWTSettings)

data HaskellEnv = Development | Production
    deriving (Show, Read, Eq)

data Environment = Environment
    { env :: HaskellEnv
    , dbConnection :: Pool Connection
    , bucket :: Text
    , minioConnection :: MinioConn
    , jwtSettings :: JWTSettings
    , serverUrl :: Text
    }
