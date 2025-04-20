module LearningJunkie.Users.Database where

import Data.Int (Int32)
import Data.Password.Bcrypt (PasswordHash (unPasswordHash), hashPassword, mkPassword)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres (Postgres)
import LearningJunkie.Database (LearningJunkieDb (dbUsers), db)
import LearningJunkie.Database.Util (executeBeamDebug, updateIfChanged)
import LearningJunkie.Universities.Database (allUniversitiesQuery, toUniversityType, universityByIdQuery)
import LearningJunkie.Universities.Database.Table
import LearningJunkie.Users.Database.Role (Role)
import LearningJunkie.Users.Database.Table
import qualified LearningJunkie.Users.User as User
import qualified LearningJunkie.Users.User.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)

type UserDBType s = (UserT (QExpr Postgres s), UniversityT (Nullable (QExpr Postgres s)))
type UserQuery s = Q Postgres LearningJunkieDb s (UserDBType s)

allUsersQuery :: UserQuery s
allUsersQuery = do
  user <- all_ $ dbUsers db
  university <-
    leftJoin_'
      allUniversitiesQuery
      (\un -> just_ (pk un) ==?. _userUniversity user)
  return (user, university)

userByIdQuery :: Int32 -> UserQuery s
userByIdQuery userId =
  filter_
    (\user -> (_userId . fst $ user) ==. val_ userId)
    allUsersQuery

userByEmailQuery :: Text -> UserQuery s
userByEmailQuery email =
  filter_
    (\user -> (_userEmail . fst $ user) ==. val_ email)
    allUsersQuery

insertUserQuery :: Attributes.New -> Role -> Maybe Int32 -> Text -> SqlInsert Postgres UserT
insertUserQuery newUser newUserRole university passwordHash =
  insert (dbUsers db) $
    insertExpressions
      [ User
          default_
          default_
          (val_ $ Attributes.name newUser)
          (val_ $ Attributes.birthday newUser)
          (val_ $ Attributes.education newUser)
          (val_ $ newUserRole)
          (val_ $ Attributes.email newUser)
          (val_ $ Attributes.avatar newUser)
          (val_ passwordHash)
          (val_ $ UniversityId $ university)
      ]

updateUserQuery :: Int32 -> Attributes.Edit -> Maybe Role -> Maybe (Maybe Int32) -> SqlUpdate Postgres UserT
updateUserQuery userId editUser editUserRole editUserUniversity =
  update
    (dbUsers db)
    ( \r ->
        updateIfChanged _userName r (Attributes.name editUser)
          <> updateIfChanged _userAvatar r (Attributes.avatar editUser)
          <> updateIfChanged _userRole r editUserRole
          <> maybe
            mempty
            (\attr -> _userUniversity r <-. val_ (UniversityId attr))
            editUserUniversity
    )
    (\r -> _userId r ==. val_ userId)

selectAllUsers :: AppM [(User, Maybe University)]
selectAllUsers =
  executeBeamDebug $
    runSelectReturningList $
      select allUsersQuery

selectUserById :: Int32 -> AppM (Maybe (User, Maybe University))
selectUserById =
  executeBeamDebug
    . runSelectReturningFirst
    . select
    . userByIdQuery

selectUserByEmail :: Text -> AppM (Maybe (User, Maybe University))
selectUserByEmail =
  executeBeamDebug
    . runSelectReturningFirst
    . select
    . userByEmailQuery

insertUser :: Attributes.New -> Role -> Maybe Int32 -> AppM (User, Maybe University)
insertUser newUser newUserRole newUserUniversity = executeBeamDebug $ do
  hashedPassword <- hashPassword . mkPassword . T.strip . Attributes.password $ newUser

  [user] <-
    runInsertReturningList $
      insertUserQuery newUser newUserRole newUserUniversity (unPasswordHash hashedPassword)

  case newUserUniversity of
    Nothing -> return (user, Nothing)
    Just universityId -> do
      university <- runSelectReturningFirst $ select $ universityByIdQuery universityId

      return (user, university)

updateUser :: Int32 -> Attributes.Edit -> Maybe Role -> Maybe (Maybe Int32) -> AppM (User, Maybe University)
updateUser userId editUser editUserRole editUserUniversity =
  executeBeamDebug $ do
    [user] <-
      runUpdateReturningList $
        updateUserQuery userId editUser editUserRole editUserUniversity

    case editUserUniversity of
      Just (Just universityId) -> do
        university <- runSelectReturningFirst $ select $ universityByIdQuery universityId

        return (user, university)
      _ -> return (user, Nothing)

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
