{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Instructors.Add (API, handler) where

import Data.Int
import LearningJunkie.Users.Database (insertUser, toUserType)
import qualified LearningJunkie.Users.Database.Role as Role
import LearningJunkie.Users.User (User)
import qualified LearningJunkie.Users.User.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermissionWithId)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Add an instructor to a university"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] User

handler :: Int32 -> AuthResult Auth.AuthUser -> Attributes.New -> AppM User
handler universityId (Authenticated authUser) newInstructor =
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameUniversity
            Permissions.Create
            Permissions.User
        )
        (Just universityId)
        $ do
            toUserType <$> insertUser newInstructor Role.Instructor (Just universityId)
handler _ _ _ = throwError err401
