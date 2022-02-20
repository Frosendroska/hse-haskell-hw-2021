import Control.Applicative (Alternative (empty, some, (<|>)))

newtype Parser a = Parser {apply :: String -> [(a, String)]}

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f
  where
    f (c : cs) | pr c = [(c, cs)]
    f cs = [(' ', cs)]

char :: Char -> Parser Char
char c = satisfy (== c)

parse :: Parser a -> String -> [a]
parse p = map fst . filter (null . snd) . apply p

instance Functor Parser where
  fmap f (Parser p) = Parser (\d -> [(f x, y) | (x, y) <- p d])

instance Applicative Parser where
  pure x = Parser (\d -> [(x, d)])
  (Parser p) <*> (Parser p') = Parser (\d -> [(f x, y) | (f, c) <- p d, (x, y) <- p' c])

instance Alternative Parser where
  empty = Parser (const [])
  p <|> p' = Parser (\d -> apply p d ++ apply p' d)

main :: IO ()
main = do
  let twoChars x = (\a b -> [a, b]) <$> char x <*> char x
  let threeChars x = (\a b c -> [a, b, c]) <$> char x <*> char x <*> char x
  print (parse (some (twoChars '7') <|> some (threeChars '7')) "777777")

-- [["77","77","77"],["777","777"]]