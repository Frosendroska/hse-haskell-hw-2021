{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Identity (Identity (..))
import Control.Monad.State -- не убирайте, используется при тестировании

newtype StrRdrT m a = StrRdrT {runStrRdrT :: String -> m a}

instance Monad m => Monad (StrRdrT m) where
  return :: a -> StrRdrT m a
  return a = StrRdrT $ \_ -> return a -- вычисление без эффекта
  -- return a = lift $ return a -- два чистых поднятия
  -- retern = lift . return

  (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
  m >>= k = StrRdrT $ \s -> do
    a <- runStrRdrT m s
    runStrRdrT (k a) s -- передаем окружение

instance MonadFail m => MonadFail (StrRdrT m) where
  fail :: String -> StrRdrT m a
  fail e = StrRdrT $ \_ -> fail e -- монада, которая умеет совершать отказы
  -- fail = rerurn . fail

instance Monad m => Functor (StrRdrT m) where
  fmap = liftM

instance Monad m => Applicative (StrRdrT m) where
  pure = return
  (<*>) = ap

--------------------------------------
askStrRdr :: Monad m => StrRdrT m String
askStrRdr = StrRdrT $ \s -> return s

asksStrRdr :: Monad m => (String -> a) -> StrRdrT m a
asksStrRdr f = StrRdrT $ \s -> return (f s)

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr r s = runIdentity $ runStrRdrT r s
--runStrRdr a = runIdentity <$> runStrRdrT a -- чистое значение

--------------------------------------
instance MonadTrans StrRdrT where
  lift :: Monad m => m a -> StrRdrT m a
  lift m = StrRdrT $ const m

--------------------------------------
instance MonadState s m => MonadState s (StrRdrT m) where
    get   = lift get
    put   = lift . put
    state = lift . state

--------------------------------------
class Monad m => MonadStrRdr m where
  askSR :: m String
  asksSR :: (String -> a) -> m a
  strRdr :: (String -> a) -> m a

instance Monad m => MonadStrRdr (StrRdrT m) where
  askSR :: StrRdrT m String
  askSR = askStrRdr
  asksSR :: (String -> a) -> StrRdrT m a
  asksSR = asksStrRdr
  strRdr :: (String -> a) -> StrRdrT m a
  strRdr f = StrRdrT $ \s -> return $ f s

instance MonadStrRdr m => MonadStrRdr (StateT s m) where
  askSR :: StateT s m String
  askSR =  lift askSR
  asksSR :: (String -> a) -> StateT s m a
  asksSR = lift . asksSR
  strRdr :: (String -> a) -> StateT s m a
  strRdr = lift . strRdr    

instance MonadIO m => MonadIO (StrRdrT m) where
    liftIO = lift . liftIO  