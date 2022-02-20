import Control.Applicative

-- instance Monoidal [] where
-- unit = [()]
-- xs *&* ys = [ (x,y) | x <- xs, y <- ys ]

-- instance Monoidal ZipList where
-- unit = ZipList (repeat ())
-- (ZipList xs) *&* (ZipList ys) = ZipList (zip xs ys)

-- GHCi> [1,2] *&* [3,4,5]
-- [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
-- GHCi> getZipList $ ZipList [1,2] *&* ZipList [3,4,5]
-- [(1,3),(2,4)]

class Functor f => Monoidal f where
  unit :: f ()
  (*&*) :: f a -> f b -> f (a, b)

instance Monoidal Maybe where
  unit = Just ()
  _ *&* Nothing = Nothing
  Nothing *&* _ = Nothing
  Just x *&* Just y = Just (x, y)

instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ())
  (a, b) *&* (x, y) = (a <> x, (b, y))

instance Monoidal ((->) e) where
  unit = mempty
  f *&* g  = \ a -> (f a, g a)

-- GHCi> Just 3 *&* Just 5
-- Just (3,5)
-- GHCi> Just 3 *&* Nothing
-- Nothing
-- GHCi> ("This is ",3) *&* ("a pair!",5)
-- ("This is a pair!",(3,5))
-- GHCi> (^2) *&* (*2) $ 5
-- (25,10)
