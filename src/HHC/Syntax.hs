module HHC.Syntax
  ( Expr (..),
    Lit (..),
    Binop (..),
    Decl,
    Program (..),
    Var,
  )
where

type Var = String

data Expr
  = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Show, Eq, Ord)

type Decl = (String, Expr)

data Program = Program [Decl] Expr deriving (Eq)
