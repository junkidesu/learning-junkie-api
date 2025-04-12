module LearningJunkie.Users.Database where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Database.Beam
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug)
import LearningJunkie.Database (LearningJunkieDb (dbUniversities, dbUsers), db, withConnection)
import LearningJunkie.Universities.Database (allUniversitiesQuery, toUniversityType)
import LearningJunkie.Universities.Database.Table (University, UniversityT)
import LearningJunkie.Users.Database.Table
import qualified LearningJunkie.Users.User as User
import LearningJunkie.Web.AppM (AppM)

type UserQuery s = Q Postgres LearningJunkieDb s (UserT (QExpr Postgres s), UniversityT (Nullable (QExpr Postgres s)))

allUsersQuery :: UserQuery s
allUsersQuery = do
    user <- all_ $ dbUsers db
    university <-
        leftJoin_'
            allUniversitiesQuery
            (\un -> just_ (pk un) ==?. _userUniversity user)
    return (user, university)

selectAllUsers :: AppM [(User, Maybe University)]
selectAllUsers = withConnection $ \conn -> do
    runBeamPostgresDebug putStrLn conn $
        runSelectReturningList $
            select allUsersQuery

toUserType :: (User, Maybe University) -> User.User
toUserType =
    User.User
        <$> _userId . fst
        <*> _userJoined . fst
        <*> _userName . fst
        <*> _userBirthday . fst
        <*> _userEducation . fst
        <*> _userRole . fst
        <*> _userEmail . fst
        <*> _userAvatar . fst
        <*> _userPasswordHash . fst
        <*> (fmap toUniversityType . snd)
