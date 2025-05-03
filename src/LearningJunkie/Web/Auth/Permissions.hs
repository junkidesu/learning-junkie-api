{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Web.Auth.Permissions where

import Control.Monad (unless)
import Data.Int (Int32)
import Data.List (find)
import LearningJunkie.Chapters.Database.Table (PrimaryKey (ChapterId))
import LearningJunkie.Courses.Database (selectCourseById)
import LearningJunkie.Courses.Database.Table (PrimaryKey (CourseId))
import LearningJunkie.Lessons.Database (selectLessonById)
import LearningJunkie.Lessons.Database.Table (LessonT (_lessonChapter))
import LearningJunkie.Universities.Database.Table (UniversityT (_universityId))
import LearningJunkie.Users.Database (selectUserById)
import LearningJunkie.Users.Database.Role
import LearningJunkie.Users.Database.Table (UserT (_userId))
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import Servant (ServerError (errBody), err401, err404, throwError)

data Scope = SameInstructor | SameUniversity | All
    deriving (Show, Read, Eq, Ord)

data Action = View | Create | Update | Delete
    deriving (Show, Read, Eq, Ord)

data Object = Course | University | User | Lesson
    deriving (Show, Read, Eq, Ord)

data Permission
    = Permission
    { scope :: Scope
    , action :: Action
    , object :: Object
    }
    deriving (Show, Read, Eq, Ord)

permissionAssignment :: Role -> [Permission]
permissionAssignment Student = []
permissionAssignment Instructor =
    [ Permission SameInstructor Update Course
    , Permission SameInstructor Create Lesson
    , Permission SameInstructor Update Lesson
    , Permission SameInstructor Delete Lesson
    , Permission SameInstructor View User
    ]
permissionAssignment UniversityRep =
    [ Permission SameUniversity Create Course
    , Permission SameUniversity Update Course
    , Permission SameUniversity Delete Course
    , Permission All Create Lesson
    , Permission All Update Lesson
    , Permission All Delete Lesson
    , Permission SameUniversity View User
    , Permission SameUniversity Create User
    , Permission SameUniversity Delete User
    , Permission SameUniversity Update University
    ]
permissionAssignment Admin =
    [ Permission All Create University
    , Permission All Update University
    , Permission All Delete University
    , Permission All Create Course
    , Permission All Update Course
    , Permission All Delete Course
    , Permission All Create Lesson
    , Permission All Update Lesson
    , Permission All Delete Lesson
    , Permission All View User
    , Permission All Create User
    , Permission All Delete User
    , Permission All Update User
    ]

checkPermission :: Permission -> Role -> Maybe Permission
checkPermission permission role = find (>= permission) rolePermissions
  where
    rolePermissions = filter (\p -> object p == object permission) $ permissionAssignment role

checkScope :: Permission -> Auth.AuthUser -> Int32 -> AppM ()
checkScope (Permission All _ _) _ _ = return ()
checkScope (Permission SameUniversity Create _) authUser objectId = do
    let sameUniversity = Auth.university authUser == Just objectId
    unless sameUniversity $ throwError err401{errBody = "Insufficient permission: operation can only be done by university rep"}
checkScope (Permission SameUniversity _ User) authUser objectId = do
    mbUser <- selectUserById objectId

    case mbUser of
        Nothing -> throwError err404
        Just (_, Nothing) -> throwError err401
        Just (_, Just university) -> do
            let sameUniversity = Just (_universityId university) == Auth.university authUser
            unless sameUniversity $ throwError err401
checkScope (Permission SameUniversity _ Course) authUser objectId = do
    mbCourse <- selectCourseById objectId

    case mbCourse of
        Nothing -> throwError err404
        Just (_, university, _) -> do
            let sameUniversity = Just (_universityId university) == Auth.university authUser
            unless sameUniversity $ throwError err401
checkScope (Permission SameUniversity _ University) authUser objectId = do
    let sameUniversity = Auth.university authUser == Just objectId
    unless sameUniversity $ throwError err401
checkScope (Permission SameInstructor _ Course) authUser objectId = do
    mbCourse <- selectCourseById objectId

    case mbCourse of
        Nothing -> throwError err404
        Just (_, _, (instructor, _)) -> do
            let sameInstructor = _userId instructor == Auth.id authUser
            unless sameInstructor $ throwError err401
checkScope (Permission SameInstructor _ Lesson) authUser objectId = do
    mbLesson <- selectLessonById objectId

    case mbLesson of
        Nothing -> throwError err404
        Just lesson -> do
            let ChapterId
                    (CourseId courseId)
                    _ = _lessonChapter lesson

            mbCourse <- selectCourseById courseId

            case mbCourse of
                Nothing -> throwError err404
                Just (_, _, (instructor, _)) -> do
                    let sameInstructor = _userId instructor == Auth.id authUser
                    unless sameInstructor $ throwError err401
checkScope _ _ _ = throwError err401

requirePermission :: Auth.AuthUser -> Permission -> AppM r -> AppM r
requirePermission authUser minPermission handler = do
    let foundPermission = checkPermission minPermission (Auth.role authUser)

    case foundPermission of
        Nothing -> throwError err401{errBody = "Insufficient permissions for your role"}
        Just _ -> do
            handler

requirePermissionWithId :: Auth.AuthUser -> Permission -> Maybe Int32 -> AppM r -> AppM r
requirePermissionWithId _ _ Nothing handler = handler
requirePermissionWithId authUser minPermission (Just objectId) handler = do
    let foundPermission = checkPermission minPermission (Auth.role authUser)

    case foundPermission of
        Nothing -> throwError err401{errBody = "Insufficient permissions for your role"}
        Just permission -> do
            checkScope permission authUser objectId
            handler
