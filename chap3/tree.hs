{- Goal : 
 - 1. Define a binary tree type that has only one constructor
 - 2. Using the binary tree type, write a function that will determine the height of the tree.
 -    The height is the largest number of hops from the root to an Empty. 
 -}

import Data.Maybe (fromMaybe)
-- 1
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) 
              deriving (Show)

-- 2
treeHeight :: Tree a -> Int
treeHeight (Node a Nothing Nothing) = 0
treeHeight (Node a (Just t1) Nothing) = 1 + (treeHeight t1)
treeHeight (Node a Nothing (Just t2)) = 1 + (treeHeight t2)
treeHeight (Node a (Just t1) (Just t2)) = do
			let h1 = treeHeight t1
			let h2 = treeHeight t2
			if h1 > h2 then 1 + h1
			else  1 + h2

-- Tests
simpleTree0 = (Node "parent") (Just simpleTree1) (Just simpleTree2)
simpleTree1 = (Node "childd") (Just simpleTree2) (Just simpleTree3)
simpleTree2 = (Node "childd") (Just simpleTree3) (Just simpleTree4)
simpleTree3 = (Node "childd") (Just simpleTree4) Nothing
simpleTree4 = (Node "childd") Nothing Nothing
simpleTreeR = (Node "Parent") Nothing (Just simpleTree1)
simpleTreeL = (Node "Parent") (Just simpleTree1) Nothing
simpleTreeE = (Node "Parent") (Just simpleTree2) (Just simpleTree1)
