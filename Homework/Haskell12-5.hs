import Control.Monad
import Control.Monad.Fail
import Control.Monad.Identity

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

instance Functor Logged where
  fmap f (Logged str a) = Logged str $ f a

instance Applicative Logged where
  pure = Logged ""
  (<*>) = ap

instance Monad Logged where
  return = Logged ""
  (>>=) (Logged s x) f = Logged (s ++ s') y
    where
      Logged s' y = f x

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (LoggT m) where
  return x = LoggT $ pure $ Logged "" x
  m >>= f = LoggT $ do
    Logged str x <- runLoggT m
    fmap (Logged str id <*>) $ runLoggT $ f x

instance MonadFail m => MonadFail (LoggT m) where
  fail msg = LoggT $ fail msg