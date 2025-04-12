module LearningJunkie.Web.AppM where

import Control.Monad.Trans.Reader (ReaderT)
import LearningJunkie.Web.Environment (Environment)
import Servant (Handler)

type AppM = ReaderT Environment Handler
