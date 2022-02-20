data Tree a = Leaf | Node (Tree a) a (Tree a) 
            deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node left middle right) = treeSum left + middle + treeSum right

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node left middle right) = 1 + max (treeHeight left) (treeHeight right)