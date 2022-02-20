data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Eq a => Eq (Tree a) where
  t' == t'' = go [t'] [t'']
    where
      go [] [] = True
      go (Leaf : xs) (Leaf : ys) = go xs ys
      go (Node l' m' r' : xs) (Node l'' m'' r'' : ys)
        | m' /= m'' = False
        | otherwise = go (xs ++ [l', r']) (ys ++ [l'', r''])
      go _ _ = undefined
