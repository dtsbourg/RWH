{- Rewrite takeWhile using
- 1. Explicit recursion
- 2. folds
- -}

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs) | p x == False = []
                    | otherwise = (x : takeWhile p xs)


