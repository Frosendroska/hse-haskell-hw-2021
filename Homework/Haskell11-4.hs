{-# LANGUAGE BlockArguments #-}

-- Функцию factorial реализовывать не надо!

import Control.Monad
import qualified Control.Monad
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Prelude hiding (init)

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  val <- readIORef ref
  when (p val) $ do
      action
      while ref p action

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM p act ref =
  when (p ref) $ act ref >>= whileM p act

factorial :: Integer -> IO Integer
factorial n = do
  r <- newIORef 1
  i <- newIORef 1
  while
    i
    (<= n)
    ( do
        ival <- readIORef i
        modifyIORef' r (* ival)
        modifyIORef' i (+ 1)
    )
  readIORef r

main :: IO ()
main = do undefined