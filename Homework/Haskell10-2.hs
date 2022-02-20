lookups :: (Eq k) => k -> [(k, v)] -> [v]
lookups x ys = do 
    a <- filter ( \(k,_) -> x == k ) ys
    return( snd a )

lookUp :: (Eq k) => k -> [(k, v)] -> v
lookUp k = foldl (\acc (k', v) -> if k == k' then v else acc) (error "key not found")

findKey :: Eq a => a -> [(a, b)] -> [(a, b)]
findKey key = filter (\(k, _) -> key == k)

fk :: (Foldable t, Eq a1) => a1 -> t (a1, [a2]) -> [a2]
fk key = foldr (\(k,v) acc -> if key == k then v else acc) []