-- Сделайте типы данных Arr2 e1 e2 и Arr3 e1 e2 e3 представителями классов типов Functor и Applicative

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}

newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

-- с естественной семантикой двух и трех окружений:

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 (\e1 e2 -> f (g e1 e2)) -- должна взять f и контейнер и всё обернуть

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 (\e1 e2 e3 -> f (g e1 e2 e3))

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\_ _ -> x)
  (<*>) (Arr2 f) (Arr2 g) = Arr2 (\e1 e2 -> f e1 e2 (g e1 e2))

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\_ _ _ -> x)
  (<*>) (Arr3 f) (Arr3 g) = Arr3 (\e1 e2 e3 -> f e1 e2 e3 (g e1 e2 e3))

main :: IO ()
main = do
  print (getArr2 (fmap length (Arr2 take)) 10 "abc")
  -- 3
  print (getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1, 2, 3, 4] [10, 20, 30, 40, 50])
  -- [33,44]
  print (getArr2 (Arr2 (\x y z -> x + y - z) <*> Arr2 (*)) 2 3)
  -- -1
  print (getArr3 (Arr3 (\x y z w -> x + y + z - w) <*> Arr3 (\x y z -> x * y * z)) 2 3 4)

-- -15
