-- revRange :: (Char, Char) -> [Char]
-- revRange (a, b) = unfoldr fun (b, False)
--   where
--     fun (x, _)
--       | x > a = Just (x, (chr (ord x - 1), False))
--       | x == a = Just (x, (x, True))
--       | otherwise = Nothing

import Data.Char
import Data.Char (Char, chr, ord)
import Data.List
import Data.List (unfoldr)
import Prelude

-- revRange :: (Char, Char) -> [Char]
-- revRange (a, b) = unfoldr fun $ ord b
--   where
--     fun x
--       | x < ord a = Nothing
--       | otherwise = Just (chr x, pred x)

-- fun2 :: Char -> Maybe (Char, Char)
-- fun2 a = if a > 'z' || False then Nothing else Just (a, chr (ord a + 1))

revRange :: (Char, Char) -> String
revRange (a, b) = unfoldr fun (a, b)

fun :: (Ord b, Enum b) => (b, b) -> Maybe (b, (b, b))
fun (x, b) = if  b >= x then Just (b, (x, pred b)) else Nothing


main :: IO ()
main = do
  print (revRange ('a', 'f'))
  print (revRange ('A', 'F'))
  print (revRange ('F', 'F'))
  print (revRange ('F', 'Q'))
  print (revRange ('Q', 'F'))
