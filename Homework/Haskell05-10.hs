-- movingLists :: Int -> [a] -> [[a]]
-- movingLists _ [] = []
-- movingLists n xx@(x : xs) = map (take n) (tails xx)

-- movingLists2 :: Int -> [a] -> [[a]]
-- movingLists2 n xx = take (length xx - n + 1) (helper2 n xx)

-- helper2 :: Int -> [a] -> [[a]]
-- helper2 _ [] = []
-- helper2 n xx@(_:xs) = take n xx : helper2 n xs

-- movingLists1 :: Int -> [a] -> [[a]]
-- movingLists1 _ [] = []
-- movingLists1 n xx@(_ : _) = take (n+1) (helper1 n xx)

-- helper1 :: Int -> [a] -> [[a]]
-- helper1 n = map (take n) . tails

isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty (_ : _) = True

longerThan :: Int -> [a] -> Bool
longerThan n xs = isNotEmpty $ drop n xs

movingLists :: Int -> [a] -> [[a]]
movingLists n xx
  | longerThan 1000000 xx = helper n xx
  | otherwise = reverse (drop n (reverse (helper n xx)))

helper :: Int -> [a] -> [[a]]
helper _ [] = [[]]
helper n xx@(_ : xs) = take n xx : helper n xs

main :: IO ()
main =
  print (movingLists 5 [5 .. 100])