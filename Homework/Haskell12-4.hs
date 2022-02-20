import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error
import Data.Foldable (msum)
import System.IO
import Data.Char (isNumber, isPunctuation)

newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

instance Semigroup PwdError where
     (PwdError e) <> _ = PwdError e

instance Monoid PwdError where
    mempty                 = PwdError ""
    mappend (PwdError e) _ = PwdError e
 
getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
    s <- liftIO getLine    
 
    let handleError (PwdError e) = liftIO $ putStrLn e >> return False
 
    validation <- isValid s `catchError` handleError
    guard validation
 
    return s
 
isValid :: String -> PwdErrorIOMonad Bool
isValid s = 
    do
        let errorPref = "Incorrect input: "

        lengthValid  <- 
            validator ((>= 8) . length) (errorPref ++ "password is too short!") s
        digitValid <- 
            validator (any isNumber) (errorPref ++ "password must contain some digits!") s
        punctValid <- 
            validator (any isPunctuation) (errorPref ++ "password must contain some punctuations!") s
 
        return $ lengthValid && digitValid && punctValid

validator :: (String -> Bool) -> String -> String -> PwdErrorIOMonad Bool
validator f errMsg s  |  f s =  return True
                      |  otherwise = throwError $ PwdError errMsg

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."


  -- ВЫЗОВ: runExceptT askPassword


{-                      
askPassword0 :: MaybeT IO ()
askPassword0 = do
    liftIO $ putStrLn "Enter your new password:"
    value <- msum $ repeat getValidPassword0
    liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
    s <- liftIO getLine
    guard (isValid0 s)
    return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s


askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."
----
-}
            
-- {-# OPTIONS_GHC -Wno-deprecations #-}
-- import Control.Monad.Trans.Except
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Foldable (msum)
-- import Data.Char (isNumber, isPunctuation)
-- import Control.Monad.Error
-- import Control.Monad.Trans.Writer

-- newtype PwdError = PwdError String
-- type PwdErrorIOMonad = ExceptT PwdError IO

-- withErr :: MonadError e m => (e -> m ()) -> m a -> m a
-- withErr f ac = catchError ac (\e -> f e >> throwError e)

-- try :: MonadError e m => (e -> m ()) -> m a -> m a
-- try h ac = go where go = catchError ac (\e -> h e >> go)

-- recordErrors :: MonadError e m => m a -> m (a, [e])
-- recordErrors = runWriterT . try (tell . (:[])) . lift

-- instance Error PwdError where
--     noMsg = PwdError "Unknown error"
--     strMsg = PwdError

-- instance Show PwdError where
--     show (PwdError s) = show s

-- getValidPassword :: PwdErrorIOMonad String
-- getValidPassword = do
--     s <- liftIO getLine
--     if length s < 8
--         then throwError $ PwdError "Incorrect input: password is too short!"
--         else if not $ any isNumber s
--             then throwError $ PwdError "Incorrect input: password must contain some digits!"
--             else if not $ any isPunctuation s
--                 then throwError $ PwdError "Incorrect input: password must contain some punctuations!"
--                 else return s

-- askPassword' :: PwdErrorIOMonad ()
-- askPassword' = do
--     liftIO $ putStrLn "Enter your new password:"
--     _ <- try (liftIO . print) getValidPassword
--     liftIO $ putStrLn "Storing in database..."
