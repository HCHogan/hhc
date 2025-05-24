module HHC.Core.Test where

import Data.Map qualified as Map
import HHC.Core.Syntax
import HHC.Core.Tc

exampleEnv :: Env
exampleEnv = Env {
  termEnv = Map.empty,
  typeEnv = Map.empty,
  tyConEnv = Map.fromList [
    (TyCon "Maybe", KFun KStar KStar), (TyCon "Either", KFun KStar (KFun KStar KStar)),
    (TyCon "Bool", KStar), (TyCon "Int", KStar), (TyCon "String", KStar), (TyCon "Char", KStar)
  ],
  conEnv = Map.fromList [
    (DataCon "Nothing", ([TypeVar "a"], [], TApp (TCon (TyCon "Maybe")) (TVar (TypeVar "a")))),
    (DataCon "Just", ([TypeVar "a"], [TVar (TypeVar "a")], TApp (TCon (TyCon "Maybe")) (TVar (TypeVar "a")))),
    (DataCon "Left", ([TypeVar "e", TypeVar "a"], [TVar (TypeVar "e")], TApp (TApp (TCon (TyCon "Either")) (TVar (TypeVar "e"))) (TVar (TypeVar "a")))),
    (DataCon "Right", ([TypeVar "e", TypeVar "a"], [TVar (TypeVar "a")], TApp (TApp (TCon (TyCon "Either")) (TVar (TypeVar "e"))) (TVar (TypeVar "a"))))
  ]
}

--   expr1 :: forall e a. e -> Maybe a -> Either e a
--   expr1 err x = case x of
--     Nothing -> Left err
--     Just y -> let z = 5 in case z of
--       5 -> Right y
--       w -> Right y
expr1 :: Expr
expr1 = TyLam (TypeVar "e") KStar $ TyLam (TypeVar "a") KStar $
  Lam (TermVar "err") (TVar (TypeVar "e")) $
  Lam (TermVar "x") (TApp (TCon (TyCon "Maybe")) (TVar (TypeVar "a"))) $
  Case (Var (TermVar "x")) [
    -- match Nothing
    Alt (PCon (DataCon "Nothing") [])
        (Con (DataCon "Left") [TVar (TypeVar "e"), TVar (TypeVar "a")] [Var (TermVar "err")]),
    -- match Just y
    Alt (PCon (DataCon "Just") [TermVar "y"])
        (Let (TermVar "z") (Lit (LInt 5))
             (Case (Var (TermVar "z")) [
               Alt (PLit (LInt 5)) (Con (DataCon "Right") [TVar (TypeVar "e"), TVar (TypeVar "a")] [Var (TermVar "y")]),
               Alt (PVar (TermVar "w")) (Con (DataCon "Right") [TVar (TypeVar "e"), TVar (TypeVar "a")] [Var (TermVar "y")])
             ]))
  ]

testExpr :: Expr
testExpr = Let (TermVar "f") expr1 $
  App (App (TyApp (TyApp (Var (TermVar "f")) (TCon (TyCon "Bool"))) (TCon (TyCon "Int")))
            (Lit (LBool False)))
      (Con (DataCon "Just") [TCon (TyCon "Int")] [Lit (LInt 5)])

-- >>> execTc exampleEnv (typeCheck testExpr)
-- Right (TApp (TApp (TCon (TyCon "Either")) (TCon (TyCon "Bool"))) (TCon (TyCon "Int")))
