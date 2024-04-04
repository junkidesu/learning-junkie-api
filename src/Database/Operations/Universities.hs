module Database.Operations.Universities (
        allUniversities,
        universityById,
        insertUniversity,
        removeUniversity,
) where

import Data.Pool (Pool)
import Database (delete, getMany_, getOne, insertReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Universities (allUniversitiesQ, insertUniversityQ, removeUniversityQ, universityByIdQ)
import Types.University (University)
import qualified Types.University.NewUniversity as NU

allUniversities :: Pool Connection -> IO [University]
allUniversities conns = getMany_ conns allUniversitiesQ

universityById :: Pool Connection -> Int -> IO (Maybe University)
universityById conns universityId = getOne conns universityByIdQ (Only universityId)

insertUniversity :: Pool Connection -> NU.NewUniversity -> IO University
insertUniversity conns = insertReturning conns insertUniversityQ

removeUniversity :: Pool Connection -> Int -> IO ()
removeUniversity conns universityId = delete conns removeUniversityQ (Only universityId)
