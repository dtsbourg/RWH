{- Goal :
 - 1. Write fromList which takes List a -> [a]
 - 2. Write a function that computes the number of elements in a list. 
 -    To test it, ensure that it gives the same answers as the standard length function.
 - 3. Write a function that computes the mean of a list
 - 4. Turn a list into a palindrome
 - 5. Write a function that determines whether its input list is a palindrome.
 - 6. Create a function that sorts a list of lists based on the length of each sublist. (sortBy)
 - 7. Define a function that joins a list of lists together using a separator value:
 -    intersperse :: a -> [[a]] -> [a]
 -}

import Data.List (sortBy)

-- 1
data List a = Cons a (List a)
	    | Nil
	    deriving (Show)

fromList :: List a -> [a]
fromList Nil           = []
fromList (Cons a (b))  = (a : fromList(b))

-- 2 (Should use fold but confusing type inference) 
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

-- 3
integralMean :: Fractional a => [a] -> a 
integralMean xs = ((foldl (+) 0 xs) / fromIntegral (len xs))

-- 4 (Does not use xs ++ reverse xs, to easy o_o)
palindrome :: [a] -> [a]
palindrome (x:xs) = ([x] ++ (palindrome xs) ++ [x])
palindrome _ = []

-- 5
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:xs) 
		| last xs /= x = False
		| otherwise    = isPalindrome $ init xs 


-- 6 : Need more info on sortBy
sortList :: [a] -> [a]
sortList (x:xs) = undefined
sortList _ = []

-- 7
intersperse :: a -> [[a]] -> [a]
intersperse s xxs = foldr (\acc x -> acc ++ (s:x)) [] xxs
