
or :: [Bool] -> Bool
or = foldr (||) False

length :: [a] -> Int
length = foldr (const succ) 0

maximum :: Ord a => [a] -> Maybe a
maximum = foldr (\x m -> maybe (Just x) (\y -> Just $ max y x) m) Nothing

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x ys -> if p x then x : ys else ys) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:).f) []

head :: [a] -> Maybe a
head = foldr (\x hm -> maybe (Just x) (const $ Just x) hm) Nothing

last :: [a] -> Maybe a
last = foldl (\lm x -> Just x) Nothing



drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n

step :: p1 -> p2 -> p3 -> a
step i xs n = undefined

ini :: b -> [a]
ini = const []