import Data.List (intercalate)

-- intercalate xs xss is equivalent to (concat (intersperse xs xss)). It inserts the list xs in between the lists in xss and concatenates the result.

newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
  show (Matrix xx)
    | null xx = showString "EMPTY" []
    | otherwise = intercalate "\n" . map show $ xx