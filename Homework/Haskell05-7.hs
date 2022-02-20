-- https://stepik.org/course/101629/syllabus

-- count :: Integer -> [Integer] -> [Integer]
-- count x yy@(y:ys) c | x == y = count x ys c + 1
--                     | yy == [] = c
--                     | otherwise count,kb x ys c 

-- helperD xx@(x : xs) yy | elem x yy = count x yy 0
--                        | otherwise = helperD xs yy

-- containsAllDigitsOnes :: Integer -> Bool
-- containsAllDigitsOnes x | containsOnes [1, 2, 3, 4, 5, 6, 7, 8, 9] (digits x) == 9 = True
--                         | otherwise = False     

import Data.List ( sort )


containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes = (== [1..9]) . sort . filter ( /= 0) . digits              


digits :: Integer -> [Integer]
digits n = reverse (helper (abs n))

helper :: Integer -> [Integer]
helper n | n <= 9 = [n]
         | otherwise = (n `mod` 10) : helper (n `div` 10)


-- main :: IO ()
-- main = do
    --  print(containsOnes [1, 2, 3, 4, 5, 6, 7, 8, 9] (digits n))


