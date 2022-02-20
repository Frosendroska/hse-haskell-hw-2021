repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem n = concatMap (helper n)

helper :: Int -> a -> [a]
helper n x | n == 0 = []
           | otherwise = x : helper (n-1) x