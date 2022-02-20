import Control.Monad
import System.Random

avgdev :: Int -> Int -> IO Double
avgdev k n = do
  l <- replicateM k $ iter n
  return $ sum l / fromIntegral k
  where
    iter i = do
      l <- replicateM i flipCoin
      return $ abs (fromIntegral i / 2 - fromIntegral (sum l))

flipCoin :: IO Int
flipCoin = randomRIO (0, 1) :: IO Int