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

data B x = Empty | Zero x | One x
  deriving (Eq, Show)

type Bin = Fix B

instance Functor B where
  fmap _ Empty = Empty
  fmap f (Zero x) = Zero $ f x
  fmap f (One x) = One $ f x

phiB :: B Int -> Int
phiB Empty = 0
phiB (One x) = x * 2 + 1
phiB (Zero x) = x * 2

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB x
  | even x = Zero $ x `div` 2
  | otherwise = One $ x `div` 2

int2bin :: Int -> Bin
int2bin = ana psiB