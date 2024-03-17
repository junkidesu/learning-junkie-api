module Database.Operations.Universities.Instructors (
        allInstructors,
        insertInstructor,
) where

import Data.Password.Bcrypt (PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Data.Pool (Pool)
import Database
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Universities.Instructors (allInstructorsQ, insertInstructorQ)
import Types.Instructor (Instructor)
import qualified Types.User.NewUser as NU

allInstructors :: Pool Connection -> Int -> IO [Instructor]
allInstructors conns universityId =
        getMany
                conns
                allInstructorsQ
                (Only universityId)

insertInstructor :: Pool Connection -> Int -> NU.NewUser -> IO Instructor
insertInstructor conns universityId newUser = do
        hashedPassword <- hashPassword . mkPassword . NU.password $ newUser

        insertReturning
                conns
                insertInstructorQ
                ( NU.name newUser
                , NU.birthday newUser
                , NU.education newUser
                , NU.email newUser
                , unPasswordHash hashedPassword
                , universityId
                )
