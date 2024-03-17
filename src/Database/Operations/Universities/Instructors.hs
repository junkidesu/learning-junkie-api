module Database.Operations.Universities.Instructors (
        allInstructors,
        insertInstructor,
) where

import Data.Password.Bcrypt (PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Database.Queries.Universities.Instructors (allInstructorsQ, insertInstructorQ)
import Types.Instructor (Instructor)
import qualified Types.User.NewUser as NU

allInstructors :: Pool Connection -> Int -> IO [Instructor]
allInstructors conns universityId =
        withResource conns $
                \conn -> query conn allInstructorsQ (Only universityId)

insertInstructor :: Pool Connection -> Int -> NU.NewUser -> IO Instructor
insertInstructor conns universityId newUser =
        withResource conns $
                \conn -> do
                        hashedPassword <- hashPassword . mkPassword . NU.password $ newUser

                        [instructor] <-
                                query
                                        conn
                                        insertInstructorQ
                                        ( NU.name newUser
                                        , NU.birthday newUser
                                        , NU.education newUser
                                        , NU.email newUser
                                        , unPasswordHash hashedPassword
                                        , universityId
                                        )
                        return instructor
