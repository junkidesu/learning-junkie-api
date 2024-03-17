module Database.Operations.Universities (allUniversities, insertUniversity) where

import Data.Pool (Pool)
import Database (getMany_, insertReturning)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Universities (allUniversitiesQ, insertUniversityQ)
import Types.University (University)
import qualified Types.University.NewUniversity as NU

allUniversities :: Pool Connection -> IO [University]
allUniversities conns = getMany_ conns allUniversitiesQ

insertUniversity :: Pool Connection -> NU.NewUniversity -> IO University
insertUniversity conns = insertReturning conns insertUniversityQ
