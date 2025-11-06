module LearningJunkie.Web.Environment where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.Beam.Postgres (Connection)
import Network.Minio (ConnectInfo, MinioConn)
import Servant.Auth.Server (JWTSettings)

data Env = Development | Production
    deriving (Show, Read)

data Environment = Environment
    { dbConnection :: Pool Connection
    , minioBucket :: Text
    , minioConnection :: MinioConn
    , jwtSettings :: JWTSettings
    , serverUrl :: Text
    , env :: Env
    }
