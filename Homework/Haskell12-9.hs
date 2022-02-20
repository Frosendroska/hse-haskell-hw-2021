{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

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

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg s = runIdentity $ runLoggT s

instance MonadTrans LoggT where
  --lift :: Monad m => m a -> LoggT m a
  lift m = LoggT $ do
    Logged mempty <$> m

instance MonadState s m => MonadState s (LoggT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadReader r m => MonadReader r (LoggT m) where
  -- ask :: m r
  ask = lift ask

  --local :: (r -> r) -> m a -> m a
  local = mapLoggT . local

  --reader :: (r -> a) -> m a
  reader = lift . reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT