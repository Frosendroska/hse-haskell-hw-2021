{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Monad.Cont
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

--  эквивалент tell
write2log' :: String -> Logged ()
write2log' s = Logged s ()

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg s = runIdentity $ runLoggT s

instance MonadTrans LoggT where
  lift :: Monad m => m a -> LoggT m a
  lift m = LoggT $ do
    Logged mempty <$> m