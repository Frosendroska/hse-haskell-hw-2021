import Control.Monad

type Board = Int

next :: Board -> [Board]
next ini = filter (>= 0) . filter (<= 9) $ [ini + 2, ini - 1] --по заданной конфигурации поля возвращает все конфигурации, достижимые из исходной за один ход

twoTurns :: Board -> [Board]
twoTurns ini = do
  bd1 <- next ini
  next bd1

threeTurns :: Board -> [Board]
threeTurns ini = do
  bd1 <- next ini
  bd2 <- next bd1
  next bd2

doNTurns :: Int -> Board -> [Board]
-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b ->   t a -> m b
--                                   ^   function  ^    ^init  ^[]    ^ res
doNTurns n ini = foldM func ini [1..n] where
          func = \x _ -> next x

main :: IO ()
main = do
  print (next 5)
  print (twoTurns 5)
  print (threeTurns 5)

--   foldM f a1 [x1, x2, ..., xm]

--     ==

--     do
--     a2 <- f a1 x1
--     a3 <- f a2 x2
--     ...
--     f am xm