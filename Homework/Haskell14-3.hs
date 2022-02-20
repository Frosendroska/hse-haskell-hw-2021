{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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

data E e = Num Int | Add e e | Mult e e

type Expr = Fix E

instance Functor E where
  fmap _ (Num a) = Num a
  fmap f (Add x y) = Add (f x) (f y)
  fmap f (Mult x y) = Mult (f x) (f y)

phiE :: E Int -> Int
phiE (Num a) = a
phiE (Add x y) = x + y
phiE (Mult x y) = x * y

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num a) = show (phiE $ Num a)
phiEShow (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
phiEShow (Mult x y) = "(" ++ x ++ "*" ++ y ++ ")"

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num a) = (show (phiE $ Num a) ++)
phiEShowS (Add x y) = (("+ " ++ x [] ++ " " ++ y []) ++)
phiEShowS (Mult x y) = (("* " ++ x [] ++ " " ++ y []) ++)

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num a) s = push a s
phiE' (Add x y) s = push (head (add $ x [] ++ y [])) s
phiE' (Mult x y) s = push (head (mult $ x [] ++ y [])) s

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'