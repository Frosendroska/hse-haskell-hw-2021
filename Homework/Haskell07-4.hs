foldl''' :: Foldable t1 => (t2 -> t3 -> t2) -> t2 -> t1 t3 -> t2
foldl''' f v xs = foldr step id xs v
    where step x g a = g (f a x)

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v
fun f x g a = g (f a x)
ini = id    