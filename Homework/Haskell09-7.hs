-- Следующий класс типов изоморфен аппликативному функтору:

-- class Functor f => Monoidal f where
--   unit  :: f ()
--   (*&*) :: f a -> f b -> f (a,b)
-- Метод unit оборачивает в контейнер что-то неинтересное, а (*&*) делает контейнер пар из пары контейнеров. Ниже приведена пара представителей

-- instance Monoidal [] where
--   unit = [()]
--   xs *&* ys = [ (x,y) | x <- xs, y <- ys ]
  
-- instance Monoidal ZipList where
--   unit = ZipList (repeat ())
--   (ZipList xs) *&* (ZipList ys) = ZipList (zip xs ys)
-- и примеры использования

-- GHCi> [1,2] *&* [3,4,5]
-- [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
-- GHCi> getZipList $ ZipList [1,2] *&* ZipList [3,4,5]
-- [(1,3),(2,4)]
-- В следующих задачах мы реализуем еще несколько представителей и формализуем упомянутый выше изоморфизм.


-- Покажите, что всякий аппликативный функтор моноидален. Для этого реализуйте функции

unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' a b = (,) <$> a <*> b
-- с сохранением подходящей семантики.