-- digits :: Integer -> [Integer]
-- digits 0 = [0]
-- digits v = helper v []
--    where helper 0 xs = xs
--          helper n xs = let (d, s) = n `divMod` 10 in helper d (s:xs)

digits :: Integer -> [Integer]
digits n = reverse (helper (abs n))

helper :: Integer -> [Integer]
helper n
  | n <= 9 = [n]
  | otherwise = (n `mod` 10) : helper (n `div` 10)

main :: IO ()
main = do
  print (digits 1234)
  print (digits 000)
  print (digits 1)
  print (digits (-981))