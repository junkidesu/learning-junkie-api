module Database.Operations.Universities (allUniversities, insertUniversity) where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.Queries.Universities (allUniversitiesQ, insertUniversityQ)
import Types.University (University)
import qualified Types.University.NewUniversity as NU

allUniversities :: Pool Connection -> IO [University]
allUniversities conns =
        withResource conns $
                \conn -> query_ conn allUniversitiesQ

insertUniversity :: Pool Connection -> NU.NewUniversity -> IO University
insertUniversity conns newUniversity = withResource conns $
        \conn -> do
                [university] <- query conn insertUniversityQ newUniversity

                return university
