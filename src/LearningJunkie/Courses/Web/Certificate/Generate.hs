{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Certificate.Generate where

import Data.Int (Int32)
import LearningJunkie.CourseCompletions.CourseCompletion (CourseCompletion)
import LearningJunkie.CourseCompletions.Database (insertCourseCompletion, toCourseCompletionType)
import qualified LearningJunkie.Courses.Course.CompletionRequirements as Req
import LearningJunkie.Courses.Database.Table (CourseT (_courseCompletionRequirements))
import LearningJunkie.Database.Util (fromJSONB)
import LearningJunkie.Progress.Database (progressByUserAndCourseIdQ, selectProgressByUserAndCourseId)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Generate a certificate if the course is completed"
        :> "certificate"
        :> "generate"
        :> JWTAuth
        :> PostCreated '[JSON] CourseCompletion

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM CourseCompletion
handler courseId (Authenticated authUser) = do
    mbProgress <- selectProgressByUserAndCourseId (Auth.id authUser) courseId

    case mbProgress of
        Nothing -> throwError err404
        Just (_, (course, _, _, totalLessonsNum, totalExercisesNum), completedLessonsNum, completedExercisesNum) -> do
            let
                completedLessonsPercentage :: Int32
                completedLessonsPercentage =
                    floor
                        ( 100 * (fromIntegral completedLessonsNum / fromIntegral totalLessonsNum) ::
                            Double
                        )

                completedExercisesPercentage :: Int32
                completedExercisesPercentage =
                    floor
                        ( 100 * (fromIntegral completedExercisesNum / fromIntegral totalExercisesNum) ::
                            Double
                        )
            let
                reqs :: Req.CompletionRequirements
                reqs = fromJSONB $ _courseCompletionRequirements course

            let
                isCourseCompleted =
                    completedLessonsPercentage >= Req.lessonPercentage reqs
                        && completedExercisesPercentage >= Req.exercisePercentage reqs

            if isCourseCompleted
                then
                    toCourseCompletionType
                        <$> insertCourseCompletion (Auth.id authUser) courseId
                else throwError err400
handler _ _ = throwError err401
