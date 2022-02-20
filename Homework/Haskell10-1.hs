{-# LANGUAGE ScopedTypeVariables #-}

import Data.ByteString (intercalate)

surround :: a -> a -> [a] -> [a]
surround b d zs = do
    z <- zs
    [b, z, d]

main :: IO ()
main = do
  print (surround '{' '}' "abcd")

-- join x c = foldl (\ acc s -> acc ++ s ++ [c]) "" x
