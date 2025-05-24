module HHC.Core.Syntax where

newtype TermVar = TermVar String
  deriving (Eq, Ord, Show)

newtype TypeVar = TypeVar String
  deriving (Eq, Ord, Show)

newtype TyCon = TyCon String
  deriving (Eq, Ord, Show)

newtype DataCon = DataCon String
  deriving (Eq, Ord, Show)

-- Kinds: * and arrow
data Kind
  = KStar
  | KFun Kind Kind
  deriving (Eq, Ord, Show)

-- System F types + ADT type constructors
data Type
  = -- | a
    TVar TypeVar
  | -- | τ1 → τ2
    TArr Type Type
  | -- | ∀(a :: κ). τ
    TForall TypeVar Kind Type
  | -- | Bool, Maybe, etc.
    TCon TyCon
  | -- | T τ
    TApp Type Type
  deriving (Eq, Show)

-- Literal, now including Bool
data Literal
  = LInt Integer
  | LChar Char
  | LString String
  | LBool Bool
  deriving (Eq, Show)

-- Patterns for case-match
data Pat
  = -- | x
    PVar TermVar
  | -- | 42, True
    PLit Literal
  | -- | Just x, (:) h t
    PCon DataCon [TermVar]
  deriving (Eq, Show)

-- Case alternatives
data Alt = Alt Pat Expr
  deriving (Eq, Show)

-- Core expressions
data Expr
  = -- | x
    Var TermVar
  | -- | λ(x:τ). e
    Lam TermVar Type Expr
  | -- | e1 e2
    App Expr Expr
  | -- | Λ(a::κ). e
    TyLam TypeVar Kind Expr
  | -- | e [τ]
    TyApp Expr Type
  | -- | let x = e1 in e2
    Let TermVar Expr Expr
  | -- | DataCon typeArgs termArgs
    Con DataCon [Type] [Expr]
  | -- | case e of alts
    Case Expr [Alt]
  | -- | literals
    Lit Literal
  deriving (Eq, Show)
