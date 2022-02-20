{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Arrow (Arrow (second))
import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char
import Data.List
import Data.Monoid
import Data.Unique

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

-- Для двух переданных типов наиболее общий унификатор или сообщение об ошибке, если унификация невозможна.  
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

-- Чистая переменная
stepTVar :: MonadState Integer m => m Type
stepTVar = do
  n <- get
  modify (+ 1)
  return $ TVar $ show n

-- Алгоритм поиска главной пары для терма бестипового лямбда-исчисления
equations :: MonadError String m => Env -> Expr -> Type -> m [(Type, Type)]
equations env' expr' t' = evalStateT (equations' env' expr' t') 1
  where
    equations' :: MonadError String m => Env -> Expr -> Type -> StateT Integer m [(Type, Type)]
    -- E(Г, x, s)
    equations' env (Var x) t = do
      t'' <- appEnv env x
      return [(t, t'')]
    -- E(Г, MN, s)
    equations' env (m :@ n) t = do
      a <- stepTVar
      e <- equations' env m (a :-> t)
      e' <- equations' env n a
      return $ e `union` e'
    -- E(Г, \x. M, s)
    equations' env (Lam v m) t = do
      a <- stepTVar
      b <- stepTVar
      e <- equations' (extendEnv env v a) m b
      return $ e `union` [(a :-> b, t)]

-- Алгоритм поиска главной пары
principlePair :: MonadError String m => Expr -> m (Env, Type)
-- a -> a = t -> t <=> a = t and a = t
principlePair expr =
  let e = Env $ makeEnv (-1 ::Int) $ freeVars expr
      s = TVar "_"
   in do
        syst <- equations e expr s
        let (a1, a2) = helper syst
        sbs <- unify a1 a2
        return (appSubsEnv sbs e, appSubsTy sbs s)
  where
    helper = foldr1 (\(x, x') (y, y') -> (x :-> y, x' :-> y'))
    makeEnv _ [] = []
    makeEnv i (x : xs) = (x, TVar $ show i) : makeEnv (i - 1) xs
