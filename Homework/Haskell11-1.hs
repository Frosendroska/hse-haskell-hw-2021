-- Используя монаду Writer, напишите функцию правой свертки вычитанием

-- minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
-- minusLoggedR = undefined
-- в которой рекурсивные вызовы сопровождались бы записью в лог, так чтобы в результате получалось такое поведение:

-- 

import Control.Monad.Writer

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR x xs | null xs = writer (x, show x)
                  | otherwise = do
                        let res = head xs - fst (runWriter $ minusLoggedR x (tail xs))
                        let a = execWriter (listen (minusLoggedR x (tail xs)))
                        tell ("(" ++ show (head xs) ++ "-" ++ a ++ ")")
                        return res


main :: IO ()
main = do
 print ( runWriter $ minusLoggedR 0 [1..3] )
-- (2,"(1-(2-(3-0)))")