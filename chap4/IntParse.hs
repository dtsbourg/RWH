{- To int -}
module IntParser where

import Data.Char (digitToInt)
import Data.List (isPrefixOf)

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                   in loop acc' xs

-- rewrite using folds
foldInt :: String -> Int
foldInt (x:xs) | x /= '-'     = fold (x:xs)
               | otherwise    = - fold xs
                 where fold y = foldl (\a x -> a*10 + digitToInt x) 0 y
foldInt [] = 0
