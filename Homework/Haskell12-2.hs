{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import qualified Control.Applicative as GHC.Base
import Control.Monad
import Control.Monad.Error
import Data.Char

data Excep a = Err String | Ok a
  deriving (Eq, Show)

--------- Monad --------
instance Monad Excep where
  return = Ok

  Ok a >>= f = f a
  Err m >>= _ = Err m

--------- MonadFail --------
instance MonadFail Excep where
  fail _ = Err "Monad.fail error."

--------- Functor --------
instance Functor Excep where
  fmap = liftM

--------- Applicative --------
instance Applicative Excep where
  pure = return
  (<*>) = ap

--------- Alternative --------
instance Alternative Excep where
  (<|>) = mplus
  empty = mzero

--------- MonadPlus --------
instance Alternative Excep => MonadPlus Excep where
  mzero           = Err "Alternative.empty error."
  Err m `mplus` _ = Err m
  _ `mplus` r     = r

--------- MonadError String --------
instance (MonadError String) Excep where
  throwError                   = Err
  (Err l) `catchError` handler = handler l
  a  `catchError` _            = a

-- тестирование
(?/) :: (MonadError String m) => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

example :: Double -> Double -> Excep String
example x y = action `catchError` return
  where
    action = do
      q <- x ?/ y
      guard (q >= 0)
      if q > 100
        then do
          100 <- return q
          undefined
        else return $ show q

--GHCi> example 5 2
--Ok "2.5"
--GHCi> example 5 0
--Ok "Division by 0."
--GHCi> example 5 (-2)
--Ok "MonadPlus.mzero error."
--GHCi> example 5 0.002
--Ok "Monad.fail error."
