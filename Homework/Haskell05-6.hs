
containsAllDigits :: Integer -> Bool
containsAllDigits n = foldr (\ p acc -> acc && (p `elem` digits n)) True [1, 2, 3, 4, 5, 6, 7, 8, 9]

digits :: Integer -> [Integer]
digits n = reverse (helper (abs n))


helper :: Integer -> [Integer]
helper n | n <= 9 = [n]
         | otherwise = (n `mod` 10) : helper (n `div` 10)