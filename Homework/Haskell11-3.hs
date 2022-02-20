import Control.Monad.State
fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0,1)

fibStep :: State (Integer,Integer) ()
fibStep = do 
    (a, b) <- get
    put (b, a+b)