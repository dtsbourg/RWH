{- Rewrite concat using folds-}

module Concat where

concat' :: [[a]] -> [a]
concat' xs = foldr (\acc x -> acc ++ x) [] xs
