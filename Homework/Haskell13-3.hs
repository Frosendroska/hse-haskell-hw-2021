import qualified Data.Bifunctor
import Control.Arrow (Arrow(second))
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

--  Подстановка типов вместо переменных типа в тип
appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just ttype -> ttype
  Nothing -> TVar v
appSubsTy sbs (s :-> t) = appSubsTy sbs s :-> appSubsTy sbs t

-- Подстановка типов вместо переменных типа в контекст
appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs (Env env) = Env $ map (second (appSubsTy sbs)) env -- можно fmap вместто second