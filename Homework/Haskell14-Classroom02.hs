{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens get set ret s = fmap (set s) (ret $ get s)

-- _1 :: Lens (a, b) a
-- _1 = lens fst (\(_, y) v -> (v, y))

-- _2 :: Lens (a, b) b
-- _2 = lens snd (\(x, _) v -> (x, v))

newtype Const a x = Const {getConst :: a}

instance Functor (Const c) where
  fmap _ (Const v) = Const v

view :: Lens s a -> s -> a
view lns s = getConst (lns Const s)

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

over :: Lens s a -> (a -> a) -> s -> s
over lns fn s = runIdentity $ lns (Identity . fn) s

set :: Lens s a -> a -> s -> s
set lns a = over lns (const a)

class Field1 s a where
  _1 :: Lens s a

class Field2 s a where
  _2 :: Lens s a

class Field3 s a where
  _3 :: Lens s a

instance Field1 (a, b) a where
    _1 = lens fst (\(x, y) z -> (z, y))

instance Field1 (a, b, c) a where
    _1 = lens (\(x, y, z) -> x) (\(x, y, z) w -> (w, y, z))

instance Field2 (a, b) b where
    _2 = lens snd (\(x, y) z -> (x, z))

instance Field2 (a, b, c) a where
    _2 = lens (\(x, y, z) -> x) (\(x, y, z) w -> (w, y, z))

instance Field3 (a, b, c) c where
    _3 = lens (\(x, y, z) -> z) (\(x, y, z) w -> (x, y, w))
