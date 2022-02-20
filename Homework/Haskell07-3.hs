
import Data.Char
import Data.Char (Char, chr, ord)
import Data.List
import Data.List (unfoldr)
import Prelude

reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' x y = y ++ [x]
ini'     = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' = flip (:)
ini'' = []