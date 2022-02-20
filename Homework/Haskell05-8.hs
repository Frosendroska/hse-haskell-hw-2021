sublist2 :: Int -> Int -> [a] -> [a]
sublist2 _ _ [] = []
sublist2 0 from x  = take from x
sublist2 from to (_:xs) = sublist2 (from-1) (to-1) xs

sublist1 :: Int -> Int -> [a] -> [a]
sublist1 n m = take $ n - m

sublist :: Int -> Int -> [a] -> [a]
sublist from to xx | from <= 0 = take to xx
                   | otherwise = take (to - from) (drop from xx)