{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

newtype Fix f = In (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)

deriving instance Eq (f (Fix f)) => Eq (Fix f)

out :: Fix f -> f (Fix f)
out (In x) = x

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata phi (In x) = phi $ fmap (cata phi) x

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana psi x = In $ fmap (ana psi) (psi x)

hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
hylo phi psi = cata phi . ana psi

data T a x = Leaf | Branch x a x deriving (Show, Eq)

type Tree a = Fix (T a)

instance Functor (T a) where
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (f l) x (f r)

-- cписок всех узлов
phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch l x r) = l ++ [x] ++ r

tree2listInorder :: Tree a -> [a]
tree2listInorder = cata phiTInorder

-- бинарное дерево поиска
psiTBST :: Ord a => Coalgebra (T a) [a] -- [a] -> T a [a]
psiTBST [] = Leaf
psiTBST (x : xs) = Branch (filter (<= x) xs) x (filter (> x) xs)

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST

sort :: Ord a => [a] -> [a]
sort = hylo phiTInorder psiTBST