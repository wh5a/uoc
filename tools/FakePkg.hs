import Apt

import System.Environment

main = do
        pkgs <- getArgs
        mapM_ createPkg pkgs
