-- Функция

divideList :: Fractional a => [a] -> a
-- divideList [] = 1
-- divideList (x : xs) = (/) x (divideList xs) --
divideList = foldr (/) 1

-- сворачивает список посредством деления. Модифицируйте ее, реализовав divideList' :: (Show a, Fractional a) => [a] -> (String,a), такую что последовательность вычислений отражается в логе:

-- Используйте аппликативный функтор пары, сохраняя близкую к исходной функции структуру реализации

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
-- divideList' []     = _
-- divideList' (x:xs) = (/) <$> _ <*> _
divideList' =
  foldr
    (\x -> (<*>) (fmap (/) ("<-" ++ show x ++ "/", x))) -- fun
    ("1.0", 1) --ini

    -- divideList' :: (Show a, Fractional a) => [a] -> (String, a)
    -- divideList' [] = ("1.0", 1)
    -- divideList' (x : xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs

main :: IO ()
main = do
  print (divideList [3, 4, 5])
  -- 3.75
  print (divideList' [3, 4, 5])

-- ("<-3.0/<-4.0/<-5.0/1.0",3.75)