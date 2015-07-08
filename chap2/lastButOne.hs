{- Ex : last but one
 - Write a function that returns the element before the last
 -}

lbo :: [a] -> a
lbo (x:[]) = undefined
lbo (x:_:[]) = x
lbo xs = last $ init xs 
