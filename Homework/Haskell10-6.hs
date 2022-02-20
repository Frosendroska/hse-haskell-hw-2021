data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

-- Она должна обеспечивать для типа OddC поведение, аналогичное поведению функции concat для списков:

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) t) = Bi x y $ concatOC t
concatOC (Bi (Bi x1 x2 x3) y t) = Bi x1 x2 $ concatOC (Bi x3 y t)
concatOC (Bi (Un x) (Bi y1 y2 y3) t) = Bi x y1 $ concatOC (Bi (Un y2) y3 t)

main :: IO ()
main = do
  print (concatOC $ Un (Un 42))
  -- Un 42
  let tst1 = Bi 'a' 'b' (Un 'c')
  let tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
  let tst3 = Bi 'i' 'j' (Un 'k')
  print (concatOC $ Bi tst1 tst2 (Un tst3))

-- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))