-- zipWith - combine elements of 2 lists pairwise
-- const x _ = x
-- drop n xs returns the suffix of xs after the first n elements, or [] if n > length xs.
-- cycle repits lists


rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
  | n >= 0 = zipWith const (drop n (cycle xs)) xs
  | otherwise = drop k xs ++ take k xs
  where
    k = (length xs + n) `mod` length xs



main :: IO ()
main = do
        print (rotate 2 "abcdefghik")
        print (rotate (-2) "abcdefghik" )
        print (rotate 1000000001 [1..10] )
        print (rotate (-2) [1..5])
        print (rotate (-2) [1..])
