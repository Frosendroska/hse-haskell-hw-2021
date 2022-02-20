data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) t) = Bi x y $ concatOC t
concatOC (Bi (Bi x1 x2 x3) y t) = Bi x1 x2 $ concatOC (Bi x3 y t)
concatOC (Bi (Un x) (Bi y1 y2 y3) t) = Bi x y1 $ concatOC (Bi (Un y2) y3 t)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) t = Bi x y t
concat3OC (Bi x1 x2 x3) y t = Bi x1 x2 $ concat3OC x3 y t
concat3OC (Un x) (Bi y1 y2 y3) t = Bi x y1 $ concat3OC (Un y2) y3 t

instance Functor OddC where
  -- fmap :: (a -> b) -> OddC a -> OddC b
  fmap f (Un x) = Un $ f x
  fmap f (Bi x y t) = Bi (f x) (f y) (f <$> t)

instance Applicative OddC where
  -- pure :: a -> OddC a
  pure = Un

  -- (<*>) :: (OddC (a -> b)) -> (OddC a) -> (OddC b)
  (Un f) <*> (Un x) = Un $ f x
  (Un f) <*> (Bi x1 x2 x3) = Bi (f x1) (f x2) (f <$> x3)
  (Bi f1 f2 f3) <*> (Un x) = Bi (f1 x) (f2 x) (f3 <*> Un x)
  (Bi f1 f2 f3) <*> (Bi x1 x2 x3) = concat3OC a b c
    where
      a = Bi (f1 x1) (f1 x2) (f1 <$> x3)
      b = Bi (f2 x1) (f2 x2) (f2 <$> x3)
      c = concatOC $ Bi (f3 <*> Un x1) (f3 <*> Un x2) (Un $ f3 <*> x3)

instance Monad OddC where
  --(>>=) :: (OddC a) -> (a -> OddC b) -> (OddC b)
  (Un x) >>= f = f x
  (Bi x y t) >>= f = concat3OC a b c
    where
      a = f x
      b = f y
      c = t >>= f

main :: IO ()
main = do
  let tst1 = Bi 10 20 (Un 30)
  let tst2 = Bi 1 2 (Bi 3 4 (Un 5))
  let tst3 = Bi (+ 1) (+ 2) $ Un (^ 2)
  -- let's create an infinitive data structure
  let tst4 = Bi 1 2 tst4

  print (do x <- tst1; y <- tst2; return (x + y))
  print (do x <- tst2; y <- tst1; return (x + y))
  -- let's try to use infinitive data structures
  print (tst3 <*> tst4)
  print (do x <- tst1; y <- tst4; return (x + y))