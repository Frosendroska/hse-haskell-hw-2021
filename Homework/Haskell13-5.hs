{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow (Arrow (second))
import Control.Monad.Except
import Data.List (nub, union, (\\))

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
freeVars (a :@ b) = freeVars a `union` freeVars b
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

-- Позволяет использовать контекст как частичную функцию из переменных в типы
appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
  Just ttype -> pure ttype
  Nothing -> throwError $ "There is no variable " <> show v <> " in the enviroment."

--  Подстановка типов вместо переменных типа в тип
appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just ttype -> ttype
  Nothing -> TVar v
appSubsTy sbs (s :-> t) = appSubsTy sbs s :-> appSubsTy sbs t

-- Подстановка типов вместо переменных типа в контекст
appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs (Env env) = Env $ map (second (appSubsTy sbs)) env

-- Композиция двух подстановок (носитель композиции является объединением носителей двух этих подстановок)
composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy left@(SubsTy s1) right@(SubsTy s2) =
  let carrier = map fst
      result = carrier s1 `union` carrier s2
   in SubsTy $ map (\symb -> (symb, appSubsTy left (appSubsTy right (TVar symb)))) result

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []
 
unify :: MonadError String m => Type -> Type -> m SubsTy
-- []
unify (TVar a) (TVar b)
  | a == b = return $ SubsTy []
-- ошибка or a -> t
unify (TVar a) t
  | a `elem` freeTVars t = throwError $ "Can't unify  (" ++ show a ++ ") with (" ++ show t ++ ")!"
  | otherwise = return $ SubsTy [(a, t)]
-- s -> s, a
unify (s1 :-> s2) (TVar a) = unify (TVar a) (s1 :-> s2)
-- s -> s, t -> t
unify (s1 :-> s2) (t1 :-> t2) = do
  let h = unify s2 t2
  case h of
    Left err -> throwError err
    Right s -> do
      let h' = unify (appSubsTy s s1) (appSubsTy s t1)
      case h' of
        Left err -> throwError err
        Right s' -> return $ composeSubsTy s' s