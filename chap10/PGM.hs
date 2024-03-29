module PGM where

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)

data Greymap = Greymap {
    greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
  } deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap {" ++ show w ++ "," ++ show h ++ "}." ++ show m

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 = undefined
