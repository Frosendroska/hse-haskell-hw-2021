
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n (x : xs) = [x : ys | ys <- comb (n - 1) xs] ++ comb n xs  -- (k+1, n+1) == (k, n) + (k+1, n)

