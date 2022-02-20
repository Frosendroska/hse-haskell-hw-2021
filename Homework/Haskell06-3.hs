class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc n
    | n == maxBound = minBound
    | otherwise = succ n

  spred :: a -> a
  spred n
    | n == minBound = maxBound
    | otherwise = pred n
