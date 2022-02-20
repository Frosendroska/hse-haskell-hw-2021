factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
  a <- filter (\x -> mod n x == 0) [1 .. round (sqrt (fromIntegral n))]
  return (a, n `div` a)
