{-# LANGUAGE InstanceSigs #-}

import Control.Monad (ap, liftM)
import Control.Monad.Identity (Identity (..))

newtype StrRdrT m a = StrRdrT {runStrRdrT :: String -> m a}

instance Monad m => Monad (StrRdrT m) where
  return :: a -> StrRdrT m a
  return a = StrRdrT $ \_ -> return a -- вычисление без эффекта

  (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
  m >>= k = StrRdrT $ \s -> do
    a <- runStrRdrT m s
    runStrRdrT (k a) s -- передаем окружение

instance MonadFail m => MonadFail (StrRdrT m) where
  fail :: String -> StrRdrT m a
  fail e = StrRdrT $ \_ -> fail e -- монада, которая умеет совершать отказы

instance Monad m => Functor (StrRdrT m) where
  fmap = liftM

instance Monad m => Applicative (StrRdrT m) where
  pure = return
  (<*>) = ap