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

class Monad m => MonadLogg m where
    w2log :: String -> m ()
    logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
    w2log = write2log
    logg  = LoggT . return

instance MonadLogg m => MonadLogg (StateT s m) where
    w2log = lift . w2log
    logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
    w2log = lift . w2log
    logg  = lift . logg

-- logSt'' :: LoggT (State Integer) Integer      
-- logSt'' = do 
--   x <- logg $ Logged "BEGIN " 1
--   modify (+x)
--   a <- get
--   w2log $ show $ a * 10
--   put 42
--   w2log " END"
--   return $ a * 100

-- rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer
-- rdrStLog = do
--     x <- logg $ Logged "BEGIN " 1
--     y <- ask
--     modify (+ (x+y))
--     a <- get
--     w2log $ show $ a * 10
--     put 42
--     w2log " END"
--     return $ a * 100