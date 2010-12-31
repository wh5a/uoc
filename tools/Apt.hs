-- Since there's no Haskell binding to libapt-pkg-dev, we parse the output of the apt commands.

-- TODO: Redirect some outputs to /dev/null. Wait on child processes to finish.

module Apt(willInstall, cacheDir, getContent, createPkg) where

import System.Process
import System.IO
import Data.List
import System.Posix.Directory

extraPrompt = "The following extra packages will be installed:"

isSubOutput = isPrefixOf "  "

-- Find out extra packages that will be pulled in by a given package
getDeps :: String -> IO [String]
getDeps pkg = do
                    (_, Just hout, _, _) <- createProcess (proc "apt-get" ["install", "-s", pkg]){ std_out = CreatePipe }
                    contents <- hGetContents hout
                    let outputs = lines contents
                        extras = dropWhile (/= extraPrompt) outputs
                    if null extras then return []
                       else let extras' = takeWhile isSubOutput $ tail extras
                                pkgs = words $ concat extras'
                            in return pkgs

willInstall pkg = do
         createProcess (proc "sudo" ["apt-get", "install", "-yd", pkg])
         getDeps pkg

cacheDir = "/var/cache/apt/archives/"

getContent deb' = let deb = cacheDir ++ deb' in do
           (_, Just hout, _, _) <- createProcess (proc "dpkg" ["-c", deb]) { std_out = CreatePipe }
           contents <- hGetContents hout
           let outputs' = lines contents
               -- No symbolic links
               outputs = filter (not . (isInfixOf "->")) outputs'
               getFile = tail . last . words
               files = map getFile outputs
           return files                             

-- Find out the version
getVersion :: String -> IO String
getVersion pkg =  do
                    (_, Just hout, _, _) <- createProcess (proc "apt-cache" ["policy", pkg]){ std_out = CreatePipe }
                    contents <- hGetContents hout
                    let outputs = lines contents
                        candidateLine = head $ filter (isInfixOf "Candidate:") outputs
                    return ((words candidateLine) !! 1)

safeMkDir dir mode = catch (createDirectory dir mode) (const $ return ())

createPkg :: String-> IO ()
createPkg pkg = do
          ver <- getVersion pkg
          safeMkDir pkg 0o755
          safeMkDir (pkg ++ "/DEBIAN") 0o755
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
