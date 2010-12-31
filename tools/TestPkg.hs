import Apt

import System.Environment
import System.Posix.Directory
import Control.Monad
import Data.List
import System.Posix.Files
import System.IO

readdir :: DirStream -> IO [String]
readdir dir = do
        filename <- readDirStream dir
        if null filename then return []
         else liftM (filename:) $ readdir dir

matchPkg pkg = head . (filter (isPrefixOf (pkg ++ "_")))

-- Not a directory, and contains lib/ or bin/
isInteresting file = (last file /= '/') && (isInfixOf "lib/" file || isInfixOf "bin/" file)

fileExists :: FilePath -> IO Bool
fileExists f = catch (getFileStatus f >> return True)
                     (const $ return False)

-- Given a package, guess if it's already installed, and let the user decide if unsure
guess :: [String] -> String -> IO Bool
guess debs pkg = do
        putStrLn $ "Handling " ++ pkg ++ "..."
        files' <- getContent $ matchPkg pkg debs
        let files = filter isInteresting files'
        exists <- mapM fileExists files
        if and exists then do
          when (isInfixOf "-dev" pkg) $
            putStrLn "  WARNING: this is a dev pkg"
          putStrLn "  exists. Creating a fake deb..."
          createPkg pkg
          return True
         else if not $ or exists then
           putStrLn "  doesn't exist." >> return False
          else do
               ans <- decide $ zip files exists
               when ans $ createPkg pkg
               return ans

decide :: [(String, Bool)] -> IO Bool
decide xs = let pp (s, True) = putStrLn $ "hit    " ++ s
                pp (s, False) = putStrLn $ "miss   " ++ s
                hits = filter snd xs
                misses = filter (not . snd) xs
                readYN = do
                            putStr "Does this package exist? (y/n/?) "
                            c <- getChar
                            putStrLn ""
                            if c == 'y' then return True
                             else if c == 'n' then return False
                              else (when (c == '?') $ mapM_ pp xs) >> readYN
            in do
               putStrLn ((show $ length hits) ++ " hits, " ++ (show $ length misses) ++ " misses.")
               mapM_ pp $ take 5 hits
               mapM_ pp $ take 5 misses
               readYN

main = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        [pkg] <- getArgs
        dir <- openDirStream cacheDir
        debs <- readdir dir
        pkgs <- willInstall pkg
        mapM_ (guess debs) pkgs
