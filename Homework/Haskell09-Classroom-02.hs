nat :: Parser Char Int
nat = (foldl (\y x -> y*10 + x) 0) <$> some digit


