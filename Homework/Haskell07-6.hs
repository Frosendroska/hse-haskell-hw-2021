data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)
instance Foldable Tree where
    foldr _ ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l


newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
instance Foldable Preorder where
    foldr _ ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
instance Foldable Postorder where
    foldr _ ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)
instance Foldable Levelorder where
    foldr f ini tree = foldr f ini (concatMap (oneLevelorder tree) [0..n])
     where n = treeHeight tree - 1

oneLevelorder :: (Eq a1, Num a1) => Levelorder a2 -> a1 -> [a2]
oneLevelorder (LevelO Nil) _ = []
oneLevelorder (LevelO (Branch l x r)) n | n == 0    = [x] 
                                        | otherwise = oneLevelorder (LevelO l) (n - 1) ++ oneLevelorder (LevelO r) (n - 1)

treeHeight :: (Num p, Ord p) => Levelorder a -> p
treeHeight (LevelO Nil) = 0
treeHeight (LevelO (Branch l _ r)) = 1 + max (treeHeight (LevelO l)) (treeHeight (LevelO r))



main :: IO ()
main = do
    let tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
    print (foldr (:) [] tree)
    print (foldr (:) [] $ PreO tree)
    print (foldr (:) [] $ PostO tree)
    print (foldr (:) [] $ LevelO tree)
