module HHC.Core.Optimizer
  ( optimizeExpr,
  )
where

import HHC.Core.Syntax

-- | Optimize a single Core expression by repeatedly applying
--   simplification and constant-folding until a fixpoint.
optimizeExpr :: Expr -> Expr
optimizeExpr expr =
  let expr' = constantFold (simplify expr)
   in if expr' == expr
        then expr
        else optimizeExpr expr'

-- | Simplify trivial let-bindings and beta-reduction
simplify :: Expr -> Expr
simplify = go
  where
    go (Let v (Lit l) body) = substitute v (Lit l) (go body)
    go (App (Lam v _ body) arg) = go (substitute v arg body)
    go (Lam v t body) = Lam v t (go body)
    go (App f a) = App (go f) (go a)
    go (TyLam a k e) = TyLam a k (go e)
    go (TyApp e ty) = TyApp (go e) ty
    go (Case e alts) = Case (go e) [(Alt p (go br)) | Alt p br <- alts]
    go (Con dc tys es) = Con dc tys (map go es)
    go other = other

-- Naive substitution (capture avoidance omitted for brevity)
substitute :: TermVar -> Expr -> Expr -> Expr
substitute v val = go
  where
    go (Var x)
      | x == v = val
      | otherwise = Var x
    go (Lam x t e)
      | x == v = Lam x t e
      | otherwise = Lam x t (go e)
    go (App f a) = App (go f) (go a)
    go (Let x e1 e2)
      | x == v = Let x (go e1) e2
      | otherwise = Let x (go e1) (go e2)
    go (TyLam a k e) = TyLam a k (go e)
    go (TyApp e ty) = TyApp (go e) ty
    go (Case e alts) = Case (go e) [Alt p (go br) | Alt p br <- alts]
    go (Con dc ts es) = Con dc ts (map go es)
    go other = other

-- | Constant folding for integer and boolean expressions
constantFold :: Expr -> Expr
constantFold = go
  where
    go (App (App (Var (TermVar op)) (Lit (LInt x))) (Lit (LInt y)))
      | op == "+" = Lit $ LInt (x + y)
      | op == "-" = Lit $ LInt (x - y)
      | op == "*" = Lit $ LInt (x * y)
    go (App (App (Var (TermVar "==")) (Lit (LInt x))) (Lit (LInt y))) =
      Lit $ LBool (x == y)
    go (App (App (Var (TermVar "&&")) (Lit (LBool x))) (Lit (LBool y))) =
      Lit $ LBool (x && y)
    go (App (App (Var (TermVar "||")) (Lit (LBool x))) (Lit (LBool y))) =
      Lit $ LBool (x || y)
    go (Lam v t e) = Lam v t (go e)
    go (App f a) = App (go f) (go a)
    go (TyLam a k e) = TyLam a k (go e)
    go (TyApp e ty) = TyApp (go e) ty
    go (Let v e1 e2) = Let v (go e1) (go e2)
    go (Case e alts) = Case (go e) [Alt p (go br) | Alt p br <- alts]
    go (Con dc ts es) = Con dc ts (map go es)
    go other = other

