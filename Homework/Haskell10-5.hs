data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) t = Bi x y t
concat3OC (Bi x1 x2 x3) y t = Bi x1 x2 $ concat3OC x3 y t
concat3OC (Un x) (Bi y1 y2 y3) t = Bi x y1 $ concat3OC (Un y2) y3 t


main :: IO ()
main = do
  let tst1 = Bi 'a' 'b' (Un 'c')
  let tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
  let tst3 = Bi 'i' 'j' (Un 'k')
  print (concat3OC tst1 tst2 tst3)

-- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))