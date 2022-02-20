data LogLevel = Error | Warning | Info
      deriving (Eq)
 {- Err > War > Inf -}

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y | x == y = EQ
        | x == Error = GT
        | y == Info = GT
        | otherwise = LT