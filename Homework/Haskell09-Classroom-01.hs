{-# LANGUAGE InstanceSigs #-}

data Result a = Ok a | Error String
  deriving (Eq, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error str) = Error str
  fmap f (Ok x)      = Ok $ f x

instance Foldable Result where
  foldMap :: Monoid m => (a -> m) -> Result a -> m
  foldMap _ (Error str) = mempty
  foldMap f (Ok x)      = f x

instance Traversable Result where
  traverse :: Applicative f => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error str) = pure $ Error str
  traverse f (Ok x)      = Ok <$> f x
   -- очень похоже на Functor, только заменяем на примернение в контексте