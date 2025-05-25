module HHC.Core.Syntax where

newtype TermVarP a = TermVar a
  deriving (Eq, Ord, Show)

newtype TypeVarP a = TypeVar a
  deriving (Eq, Ord, Show)

newtype TyConP a = TyCon a
  deriving (Eq, Ord, Show)

newtype DataConP a = DataCon a
  deriving (Eq, Ord, Show)

-- Kinds: * and arrow
data Kind
  = KStar
  | KFun Kind Kind
  deriving (Eq, Ord, Show)

-- System F types + ADT type constructors
data TypeP a
  = -- | a
    TVar (TypeVarP a)
  | -- | τ1 → τ2
    TArr (TypeP a) (TypeP a)
  | -- | ∀(a :: κ). τ
    TForall (TypeVarP a) Kind (TypeP a)
  | -- | Bool, Maybe, etc.
    TCon (TyConP a)
  | -- | T τ
    TApp (TypeP a) (TypeP a)
  deriving (Eq, Show)

-- Literal, now including Bool
data Literal
  = LInt Integer
  | LChar Char
  | LString String
  | LBool Bool
  deriving (Eq, Show)

-- PatPterns for case-match
data PatP a
  = -- | x
    PVar (TermVarP a)
  | -- | 42, True
    PLit Literal
  | -- | Just x, (:) h t
    PCon (DataConP a) [TermVarP a]
  deriving (Eq, Show)

-- Case alternatives
data AltP a = Alt (PatP a) (ExprP a)
  deriving (Eq, Show)

-- Core expressions
data ExprP a
  = -- | x
    Var (TermVarP a)
  | -- | λ(x:τ). e
    Lam (TermVarP a) (TypeP a) (ExprP a)
  | -- | e1 e2
    App (ExprP a) (ExprP a)
  | -- | Λ(a::κ). e
    TyLam (TypeVarP a) Kind (ExprP a)
  | -- | e [τ]
    TyApp (ExprP a) (TypeP a)
  | -- | let x = e1 in e2
    Let (TermVarP a) (ExprP a) (ExprP a)
  | -- | DataCon typeArgs termArgs
    Con (DataConP a) [TypeP a] [ExprP a]
  | -- | case e of alts
    Case (ExprP a) [AltP a]
  | -- | literals
    Lit Literal
  deriving (Eq, Show)

type TermVar = TermVarP String
type TypeVar = TypeVarP String
type TyCon = TyConP String
type DataCon = DataConP String
type Pat = PatP String
type Expr = ExprP String
type Alt = AltP String
type Type = TypeP String
