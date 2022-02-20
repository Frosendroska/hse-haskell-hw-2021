{-# LANGUAGE FlexibleContexts #-}

import Data.List ( nub, (\\), union )

infixl 4 :@

infixr 3 :->

type Symb = String

-- Терм
data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

-- Тип
data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

-- Контекст
newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

-- Возвращает список свободных переменных терма
freeVars :: Expr -> [Symb]
freeVars (Var name) = [name]
freeVars (m :@ n) = freeVars m `union` freeVars n
freeVars (Lam name body) = freeVars body \\ [name] -- filter (/= name) (freeVars body)

-- Возвращает список свободных переменных типа (в STT все переменные типа свободные)
freeTVars :: Type -> [Symb]
freeTVars (TVar name) = [name]
freeTVars (s :-> t) = freeTVars s `union` freeTVars t

-- Расширяет контекст переменной с заданным типом
extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) name ttype = Env (env ++ [(name, ttype)])

-- Возвращает список свободных типовых переменных контекста
freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = nub $ concatMap (\(_, t) -> freeTVars t) env