-- Since there's no Haskell binding to libapt-pkg-dev, we parse the output of the apt commands.

module Apt(willInstall, getVersion) where

import System.Process
import System.IO
import Data.List

extraPrompt = "The following extra packages will be installed:"

isSubOutput = isPrefixOf "  "

-- Find out extra packages that will be pulled in by a given package
willInstall :: String -> IO [String]
willInstall pkg = do
                    (_, Just hout, _, _) <- createProcess (proc "apt-get" ["install", "-s", pkg]){ std_out = CreatePipe }
                    contents <- hGetContents hout
                    let outputs = lines contents
                        extras = dropWhile (/= extraPrompt) outputs
                    if null extras then return []
                       else let extras' = takeWhile isSubOutput $ tail extras
                                pkgs = words $ concat extras'
                            in return pkgs

-- Given a package, guess if it's already installed, and let the user decide
guess :: String -> IO Bool
guess pkg = undefined

-- Find out the version
getVersion :: String -> IO String
getVersion pkg =  do
                    (_, Just hout, _, _) <- createProcess (proc "apt-cache" ["policy", pkg]){ std_out = CreatePipe }
                    contents <- hGetContents hout
                    let outputs = lines contents
                        candidateLine = head $ filter (isInfixOf "Candidate:") outputs
                    return ((words candidateLine) !! 1)
