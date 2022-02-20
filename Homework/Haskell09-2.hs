data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

-- функтором и аппликативным функтором, реализовав в последнем случае семантику применения узла к соответствующему узлу второго дерева:

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
  pure x = Branch (pure x) x (pure x)

  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Branch f g h) <*> (Branch l x r) = Branch (f <*> l) (g x) (h <*> r)

instance Foldable Tree where -- копипаст с решения позапрошлого дз
  foldr _ ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

main :: IO ()
main = do
  let t1 = Branch (Branch Nil 7 Nil) 2 Nil
  let t2 = Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil)
  print ((*) <$> t1 <*> t2)
  -- Branch (Branch Nil 21 Nil) 8 Nil
  print (Branch (Branch Nil (+ 3) Nil) (* 2) Nil <*> Branch Nil 7 (Branch Nil 5 Nil))

-- Branch Nil 14 Nil