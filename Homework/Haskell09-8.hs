import Control.Applicative
import Data.Functor

class Functor f => Monoidal f where
  unit :: f ()
  (*&*) :: f a -> f b -> f (a, b)

-- unit *&* v = v
-- u *&* unit = u
-- u *&* (v *&* w) = (u *&* v) *&* w

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- pure id <*> v = v
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u
-- u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

pure' :: Monoidal f => a -> f a
pure' x = x <$ unit -- pure из init

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' x y = uncurry id <$> (x *&* y)