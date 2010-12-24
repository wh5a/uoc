import Apt

import System.Environment
import Control.Monad

main = do
        [pkg] <- getArgs
        print =<< willInstall pkg
