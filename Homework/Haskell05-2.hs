{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- data Person = Person { firstName :: String, lastName :: String, age :: Int }
--             deriving (Eq, Show)

-- abbrFirstName :: Person -> Person
--     abbrFirstName p @ Person{ firstName = (x:_:_) } = p { firstName = x : "." }
--     abbrFirstName p = p


data Person = Person { firstName :: String, lastName :: String, age :: Int }
        deriving (Eq, Show)

abbrFirstName :: Person -> Person
abbrFirstName p@Person{ firstName = x : _ : _ } = p { firstName = x : "." }
abbrFirstName p = p


main :: IO ()
main = do
  let p = Person {firstName = "A", lastName = "Smith", age = 66}
  print(abbrFirstName p)