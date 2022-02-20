import Control.Applicative
import Data.List

--ZipList
(>*<) :: [a -> b] -> [a] -> [b]
x >*< y = getZipList ( ZipList x <*> ZipList y ) -- <*> :: f (a -> b) -> fa -> fb

--getZipList
(>$<) :: (a -> b) -> [a] -> [b]
f >$< x = getZipList ( f <$> ZipList x ) -- <$> :: (a -> b) -> fa -> fb
-- (>$<) = (<$>)

main :: IO ()
main = do
  -- В модуле Data.List имеется семейство функций zipWith, zipWith3, zipWith4,..:

  let [x1s, x2s, x3s, x4s] = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]

  print (zipWith (\a b -> 2 * a + 3 * b) x1s x2s)
  -- [14,19,24]
  print (zipWith3 (\a b c -> 2 * a + 3 * b + 5 * c) x1s x2s x3s)
  -- [49,59,69]
  print (zipWith4 (\a b c d -> 2 * a + 3 * b + 5 * c -4 * d) x1s x2s x3s x4s)
  -- [9,15,21]

  -- Аппликативные функторы могут заменить всё это семейство

  print (getZipList $ (\a b -> 2 * a + 3 * b) <$> ZipList x1s <*> ZipList x2s)
  -- [14,19,24]
  print (getZipList $ (\a b c -> 2 * a + 3 * b + 5 * c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s)
  -- [49,59,69]
  print (getZipList $ (\a b c d -> 2 * a + 3 * b + 5 * c -4 * d) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s <*> ZipList x4s)
  -- [9,15,21]

  -- Реализуйте операторы (>*<) и (>$<), позволяющие спрятать упаковку ZipList и распаковку getZipList:

  print ((\a b -> 2 * a + 3 * b) >$< x1s >*< x2s)
  -- [14,19,24]
  print ((\a b c -> 2 * a + 3 * b + 5 * c) >$< x1s >*< x2s >*< x3s)
  -- [49,59,69]
  print ((\a b c d -> 2 * a + 3 * b + 5 * c -4 * d) >$< x1s >*< x2s >*< x3s >*< x4s)
  -- [9,15,21]