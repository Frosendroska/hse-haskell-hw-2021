sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []

sum3 [] [] (z:zs) = (0 + 0 + z) : sum3 [] [] zs
sum3 (x:xs) [] [] = (x + 0 + 0) : sum3 xs [] []
sum3 [] (y:ys) [] = (0 + y + 0) : sum3 [] ys []

sum3 (x:xs) (y:ys) [] = (x + y + 0) : sum3 xs ys []
sum3 (x:xs) [] (z:zs) = (x + 0 + z) : sum3 xs [] zs
sum3 [] (y:ys) (z:zs) = (0 + y + z) : sum3 [] ys zs

sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs

