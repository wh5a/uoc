import Apt

import System.Environment

main = do
        [pkg] <- getArgs
        print =<< willInstall pkg
