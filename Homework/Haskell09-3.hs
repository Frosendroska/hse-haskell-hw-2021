import Control.Applicative

-- Сделайте тип

newtype Cmps f g x = Cmps {getCmps :: f (g x)} deriving (Eq, Show)

-- представителем класса типов Applicative.

-- Cmps :: (* -> *) -> (* -> *) -> * -> *
instance (Functor f, Functor g) => Functor (Cmps f g) where
  -- (a -> b) -> (Cmps f g) a -> (Cmps f g) b
  fmap h (Cmps x) = Cmps (fmap (fmap h) x)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure x = Cmps (pure (pure x))

  -- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  Cmps f <*> Cmps x = Cmps (liftA2 (<*>) f x)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldMap f (Cmps x) = foldMap (foldMap f) x


main :: IO ()
main = do
  print (getCmps $ (+) <$> Cmps [Just 1, Just 2] <*> Cmps [Nothing, Just 40])

-- [Nothing,Just 41,Nothing,Just 42]