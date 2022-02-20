-- Следующий тип данных задает гомогенную тройку элементов, которую можно рассматривать как трехмерный вектор:
data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure a = Tr a a a

  (Tr f g h) <*> (Tr a b c) = Tr (f a) (g b) (h c)

main :: IO ()
main = do
  -- Сделайте этот тип функтором и аппликативным функтором с естественной для векторов семантикой, подобной ZipList.

  print ((^ 2) <$> Tr 1 (-2) 3)
  -- Tr 1 4 9
  print (Tr (^ 2) (+ 2) (* 3) <*> Tr 2 3 4)
  -- Tr 4 5 12