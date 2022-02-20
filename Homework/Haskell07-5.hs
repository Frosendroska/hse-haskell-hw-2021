infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n 

fun :: (Num t, Ord t) => a -> (t -> Maybe a) -> t -> Maybe a
fun x s i | i == 0    = Just x
          | i < 0     = Nothing
          | otherwise = s (i - 1)

ini :: b -> Maybe a
ini = const Nothing

main :: IO ()
main = do
    print ("abcdefgh" !!! 100)
    print ("abcdefgh" !!! (-1))
    print ("abcdefgh" !!! 5)