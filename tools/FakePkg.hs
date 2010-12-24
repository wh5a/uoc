import Apt

import System.Environment
import System.Posix.Directory
import System.IO
import System.Process

main = do
        pkgs <- getArgs
        vers <- mapM getVersion pkgs
        mapM_ createPkg $ zip pkgs vers

createPkg :: (String, String) -> IO ()
createPkg (pkg, ver) = do
          createDirectory pkg 0o755
          createDirectory (pkg ++ "/DEBIAN") 0o755
          withFile (pkg ++ "/DEBIAN/control") WriteMode (writeControl pkg ver)
          createProcess (proc "dpkg" ["--build", pkg])
          return ()

writeControl :: String -> String -> Handle -> IO ()
writeControl pkg ver h = do
             hPutStrLn h $ "Package: " ++ pkg
             hPutStrLn h $ "Version: " ++ ver
             hPutStrLn h $ "Architecture: all\n\
                           \Maintainer: Fake deb generator\n\
                           \Description: Fake " ++ pkg
