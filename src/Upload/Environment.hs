module Upload.Environment (S3Environment (..)) where

import Aws (Configuration, NormalQuery)
import Aws.S3 (S3Configuration)
import Network.HTTP.Conduit (Manager)

data S3Environment
    = S3Environment
        Configuration
        (S3Configuration NormalQuery)
        Manager
