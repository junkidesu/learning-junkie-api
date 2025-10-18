{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Update (API, handler) where

import Data.Int (Int32)
import LearningJunkie.Universities.Database (toUniversityType, updateUniversity)
import LearningJunkie.Universities.University (University)
import qualified LearningJunkie.Universities.University.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Update university by ID"
        :> JWTAuth
        :> Capture' '[Required, Description "ID of the university"] "id" Int32
        :> ReqBody '[JSON] Attributes.Edit
        :> Put '[JSON] University

handler :: AuthResult Auth.AuthUser -> Int32 -> Attributes.Edit -> AppM University
handler (Authenticated authUser) universityId editUniversity =
    Permissions.requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameUniversity
            Permissions.Update
            Permissions.University
        )
        (Just universityId)
        $ toUniversityType
            <$> updateUniversity
                universityId
                editUniversity
handler _ _ _ = throwError err401
