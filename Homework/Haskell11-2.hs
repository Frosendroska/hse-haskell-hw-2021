import Control.Monad.Writer

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL x = go (writer (x, show x))
  where
    go e [] = e
    go e es = go (writer (ans - head es, "(" ++ a ++ "-" ++ show (head es) ++ ")")) $ tail es
      where
        ans = fst $ runWriter e
        a = execWriter (listen e)

-- import Control.Monad.Writer

-- minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
-- minusLoggedL x xs | null xs = writer (x, show x)
--                   | otherwise = do
--                         let res = head xs - fst (runWriter $ minusLoggedL x (tail xs))
--                         let a = execWriter (listen (minusLoggedL x (tail xs)))
--                         tell ("(" ++ a ++ "-" ++ show (head es) ++ ")")
--                         return res

main :: IO ()
main = do
  print (runWriter $ minusLoggedL 0 [1 .. 3])

-- (-6,"(((0-1)-2)-3)")