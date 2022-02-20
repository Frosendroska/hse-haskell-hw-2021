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

--  Подстановка типов вместо переменных типа в тип
appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy sbs) (TVar v) = case lookup v sbs of
  Just ttype -> ttype
  Nothing -> TVar v
appSubsTy sbs (s :-> t) = appSubsTy sbs s :-> appSubsTy sbs t

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