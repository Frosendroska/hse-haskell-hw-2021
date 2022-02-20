data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show
  
instance Functor Tree where
    fmap _ Leaf           = Leaf
    fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)