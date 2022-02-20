{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except

infixl 4 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

-- Позволяет использовать контекст как частичную функцию из переменных в типы
appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of 
        Just ttype -> pure ttype
        Nothing -> throwError $ "There is no variable "<> show v <>" in the enviroment."