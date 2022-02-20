import Control.Monad

-- Рукописный логгер
data Logged a = Logged String a deriving (Eq, Show)

instance Functor Logged where
  fmap = liftM

instance Applicative Logged where
  pure = return
  (<*>) = ap

instance Monad Logged where
  return = Logged ""
  (>>=) (Logged s x) f = Logged (s' ++ s) y
    where
      Logged s' y = f x

-- эквивалент tell
write2log :: String -> Logged ()
write2log s = Logged s ()


logIt :: Show b => b -> Logged b
logIt v = do
  write2log $ "var = " ++ show v ++ "; "
  return v

test :: Logged Integer
test = do
  x <- logIt 3
  y <- logIt 5
  let res = x + y
  write2log $ "sum = " ++ show res ++ "; "
  return res