import Control.Monad
import System.Random
import Control.Monad.State

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (x,y) = do
    gen <- get
    let (a, gen') = randomR (x, y) gen
    put gen'
    return a

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
  l <- replicateM k $ iter n
  return $ sum l / fromIntegral k
  where
    iter i = do
      l <- replicateM i flipCoin
      return $ abs (fromIntegral i / 2 - fromIntegral (sum l))

flipCoin :: State StdGen Int      
flipCoin = randomRState (0::Int, 1::Int)

-- flipCoin' :: Num a => RandomR a  
-- flipCoin' = randomRState (0, 1)



-- avgdev' :: Int -> Int -> State StdGen Double
-- avgdev' k n = fmap avg $ replicateM k $ fmap diff $ replicateM n $ randomRState (0::Int, 1::Int)
--               where th   = fromIntegral n / 2
--                     avg xs = sum xs / fromIntegral k
--                     diff =  abs . (th-) . fromIntegral . sum