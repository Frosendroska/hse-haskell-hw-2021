import Control.Monad.Except
import Data.Char

data ParseError = ParseError {location :: Int, reason :: String}

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c i
  | isHexDigit c = return (toInteger (digitToInt c))
  | otherwise    = throwError (ParseError i ([c] ++ ":" ++ " invalid digit"))

-- пытается разобрать переданную ей строку как шестнадцатеричное число  
parseHex :: String -> ParseMonad Integer
parseHex s = helper s 0 1
  where
    helper [] val _       = return val
    helper (c : cs) val i = do
                d <- parseHexDigit c i
                helper cs (val * 16 + d) (i + 1)

-- выводит информацию об этом исключении в удобном текстовом виде                
printError :: ParseError -> ParseMonad String
printError e = return $ "At pos " ++ show (location e) ++ ": " ++ reason e

-- тестирование
test s = str where
  (Right str) = do 
      n <- parseHex s
      return $ show n  
    `catchError` printError