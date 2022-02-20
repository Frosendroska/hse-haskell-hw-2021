{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)

type TreeZ a = (a, CntxT a)

data CntxT a = CntxT (Tree a) (Tree a) [(Dir, a, Tree a)]
  deriving (Eq, Show)

data Dir = L | R deriving (Eq, Show)

mktz :: Tree a -> TreeZ a
mktz (Node l x r) = (x, CntxT l r [])

left :: TreeZ a -> TreeZ a
left (x, CntxT (Node l' x' r') r s) = (x', CntxT l' r' ((L, x, r) : s))

right :: TreeZ a -> TreeZ a
right (x, CntxT l (Node l' x' r') s) = (x', CntxT l' r' ((R, x, l) : s))

up :: TreeZ a -> TreeZ a
up (x, CntxT l r ((L, x', r') : s)) = (x', CntxT (Node l x r) r' s)
up (x, CntxT l r ((R, x', l') : s)) = (x', CntxT l' (Node l x r) s)

untz :: TreeZ a -> Tree a
untz (x, CntxT l r []) = Node l x r
untz z = untz (up z)

updTZ :: a -> TreeZ a -> TreeZ a
updTZ y (x, c) = (y, c)