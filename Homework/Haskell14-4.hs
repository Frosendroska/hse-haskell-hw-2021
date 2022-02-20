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

data T a x = Leaf | Branch x a x

type Tree a = Fix (T a)

instance Functor (T a) where
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (f l) x (f r)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch l x r) = l + x + r

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum