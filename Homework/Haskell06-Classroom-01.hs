data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

elemTree :: Eq a => a -> Tree a -> Bool
elemTree x tree = go [tree]
  where
    go [] = False
    go (Leaf : xs) = go xs
    go (Node l m r : xs) = x == m || go (xs ++ [l, r])

t1 = Node (Node (Node Leaf 5 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)

t2 = Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)

t3 = Node Leaf 42 Leaf

tInf1 n = Node (tInf1 (n + 2)) n (Node Leaf 42 Leaf)

tInf2 n = Node (tInf2 (n + 2)) n (tInf2 (3 * n -1))
