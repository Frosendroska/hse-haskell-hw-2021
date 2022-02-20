absDiff :: Num a => [a] -> [a]
absDiff xs = do 
    a <- zipWith (-) as bs
    return (abs a)
    where as | null xs = []
             | otherwise = init xs
          bs | null xs = []
             | otherwise = tail xs


