{- Reproduce words but with a predicate. Splits the list at every element
- for which the predicate returns False
-}

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p []     = [[]]
splitWith p (x:xs) | p x == False = xs  : (splitWith p xs)
                   | otherwise    = [x] : (splitWith p xs)
