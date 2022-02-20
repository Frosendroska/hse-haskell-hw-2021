import Control.Monad
import Control.Monad.State
-- randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
-- randomRState (x,y) = do
--     gen <- get
--     let (a, gen') = randomR (x, y) gen
--     put gen'
--     return a

-- avgdev'' :: Int -> Int -> Double
-- avgdev'' k n = do
--   l <- replicateM k $ iter n
--   return $ sum l / fromIntegral k
--   where
--     iter i = do
--       l <- replicateM i flipCoin
--       return $ abs (fromIntegral i / 2 - fromIntegral (sum l))

-- flipCoin :: State StdGen Int
-- flipCoin = randomRState (0::Int, 1::Int)

import System.Random

gen :: StdGen
gen = mkStdGen 622

flipCoin :: [Int]
flipCoin = randomRs (0, 1) gen

avgdev'' :: Int -> Int -> Double
avgdev'' k n = helper k n flipCoin 0 n / fromIntegral k

helper :: Int -> Int -> [Int] -> Int -> Int -> Double
helper 0 _ _ _ _ = 0
helper _ _ [] _ _ = 0
helper k 0 (_ : xs) summ n' = abs (fromIntegral n' / 2 - fromIntegral summ) + helper (k - 1) n' xs 0 n'
helper k n (x : xs) summ n' = helper k (n - 1) xs (x + summ) n'