{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Create (API, handler) where

import LearningJunkie.Universities.Database (insertUniversity, toUniversityType)
import LearningJunkie.Universities.University (University)
import qualified LearningJunkie.Universities.University.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermission)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Add a university"
        :> JWTAuth
        :> ReqBody' '[Required] '[JSON] Attributes.New
        :> PostCreated '[JSON] University

handler :: AuthResult Auth.AuthUser -> Attributes.New -> AppM University
handler (Authenticated authUser) newUniversity = requirePermission
    authUser
    ( Permissions.Permission
        Permissions.All
        Permissions.Create
        Permissions.University
    )
    $ do
        toUniversityType <$> insertUniversity newUniversity
handler _ _ = throwError err401
