{- Glob.hs
 - Replicating UNIX-glob. Returns all files from directory that match glob.
 -}

module Glob (namesMatching) where

{-EXT-}
import System.Directory (doesDirectoryExist, doesFileExist,
                        getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle)
import Control.Monad (forM, foldM)
import GlobRegex (matchesGlob)

{-API-}
namesMatching :: FilePath -> IO [String]
namesMatching p | not (isPattern p) = do
            exists <- doesNameExist p
            return (if exists then [p] else [])
                | otherwise = do
      case splitFileName p of
           ("",name) -> do
             currentDir <- getCurrentDirectory
             listMatches currentDir name
           (dir,base) -> do
             dirs <- if isPattern dir
                        then namesMatching (dropTrailingPathSeparator dir)
                        else return [dir]
             let listDir = if isPattern base
                            then listMatches
                            else listPlain
             pathNames <- forM dirs $ \d -> do
                            bs <- listDir d base
                            return (map (d </>) bs)
             return (concat pathNames)


{-HELPERS-}
-- Does glob require regex ?
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

-- Abstract over File or Directory
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
       then return True
       else doesDirectoryExist name

-- Is hidden file ?
isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False

-- returns a list of all files matching the given glob pattern in a directory
listMatches :: FilePath -> String -> IO [String]
listMatches dir pat = do
  dir' <- if null dir
          then getCurrentDirectory
          else return dir
  handle ((const (return [])) :: IOError -> IO [String]) $ do -- Returns a [] if exception
    names <- getDirectoryContents dir'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (`matchesGlob` pat) names')

-- returns either an empty or singleton list, depending on whether the single name it’s passed exists
listPlain :: FilePath -> String -> IO [String]
listPlain dir base = do
  exists <- if null base
               then doesDirectoryExist dir
               else doesNameExist (dir </> base)
  return (if exists then [base] else [])

