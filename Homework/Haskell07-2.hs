-- tails' :: [a] -> [[a]]
-- tails' = foldr (\a b -> (a : head b) : b) [[]]

-- inits' :: [a] -> [[a]]
-- inits' = foldr (\a b -> [] : map (a :) b) [[]]

tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun a b = (a : head b) : b
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' a b = [] : map (a :) b
ini' = [[]]
