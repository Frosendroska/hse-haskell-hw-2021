import Data.Complex

newtype Cmplx = Cmplx (Complex Double)
  deriving (Eq)

instance Show Cmplx where
  show (Cmplx a) = show (realPart a)  ++ ( if imagPart a > 0
                                           then "+i*" ++ show (imagPart a)
                                           else "-i*" ++ show (abs (imagPart a))
                                         )

instance Read Cmplx where
  readsPrec _ s = [(Cmplx (a :+ b), b')    | (a, '+' : 'i' : '*' : a') <- reads s,
                                             (b, b')                   <- reads a'
                  ]
                  ++
                  [(Cmplx (a :+ (-b)), b') | (a, '-' : 'i' : '*' : a') <- reads s,
                                             (b, b')                   <- reads a'
                  ]


-- where
--   real = takeWhile (/= "i*") -- take (length real - 1) real
--   sign = takeWhile (/= "i*") -- drop (length sign - 1) sign
--   imag = tail . dropWhile (/= '*')

main :: IO ()
main = do
  print (Cmplx $ (-2.7) :+ 3.4) -- -2.7+i*3.4
  print (Cmplx $ (-2.7) :+ (-3.4)) -- -2.7-i*3.4