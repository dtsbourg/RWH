module RecursiveContents (getDirectoryContentsR) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getDirectoryContentsR :: FilePath -> IO [FilePath]
getDirectoryContentsR dir = do
  names <- getDirectoryContents dir
  let fNames = filter (`notElem` [".",".."]) names
  paths <- forM fNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
       then getDirectoryContentsR path
       else return [path]
  return (concat paths)

