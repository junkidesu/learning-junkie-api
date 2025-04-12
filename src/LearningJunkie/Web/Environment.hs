module LearningJunkie.Web.Environment where

import Data.Pool (Pool)
import Database.Beam.Postgres (Connection)

data Environment = Environment
    { dbConnection :: Pool Connection
    }
