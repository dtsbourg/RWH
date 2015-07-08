module Predicate where

import RecursiveContents
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException(..))
import System.IO (IOMode(..), hClose, hFileSize, openFile)

type Predicate = FilePath         -- path to directory entry
               ->Permissions      -- permissions
               ->Maybe Integer          -- file size (nothing if dir)
               ->UTCTime          -- last modified
               ->Bool

{-API-}
find :: Predicate -> FilePath -> IO [FilePath]
find p path = getDirectoryContentsR path >>= filterM check
  where check n = do
          perms <- getPermissions n
          size <- getFileSize n
          modTime <- getModificationTime n
          return (p path perms size modTime)

{-HELPER-}
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(SomeException _) ->return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h ->do
    size <- hFileSize h
    return (Just size)

{-PREDICATES-} -- Predicate = InfoP Bool
type InfoP a = FilePath
             ->Permissions
             ->Maybe Integer
             ->UTCTime
             ->a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

-- Predicate constructor (Higher order)
-- Unary operators
liftP :: (a ->b ->c) ->InfoP a ->b ->InfoP c
liftP q f k w x y z = f w x y z `q` k

-- Binary operators
liftP2 :: (a ->b ->c) ->InfoP a ->InfoP b ->InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

-- Predicates
lesserP :: (Ord a) =>InfoP a ->a ->Predicate
lesserP = liftP (<)

greaterP:: (Ord a) =>InfoP a ->a ->Predicate
greaterP = liftP (>)

equalP :: (Eq a) =>InfoP a ->a ->Predicate
equalP = liftP (==)

andP :: Predicate ->Predicate ->Predicate
andP = liftP2 (&&)

orP :: Predicate ->Predicate ->Predicate
orP = liftP2 (||)

liftPath :: (FilePath ->a) ->InfoP a
liftPath f w _ _ _ = f w

{-OPERATORS-}
(==?) :: (Eq a) =>InfoP a ->a ->Predicate
(==?) = equalP
infix 4 ==?

(&&?) :: Predicate ->Predicate ->Predicate
(&&?) = andP
infixr 3 &&?

(||?) :: Predicate ->Predicate ->Predicate
(||?) = orP
infixr 2 ||?

(>?) :: (Ord a) =>InfoP a ->a ->Predicate
(>?)  = greaterP
infix 4 >?

(<?) :: (Ord a) =>InfoP a ->a ->Predicate
(<?)  = greaterP
infix 4 <?


--myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)
