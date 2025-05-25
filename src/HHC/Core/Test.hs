module HHC.Core.Test(prettyExpr1, prettyTestExpr, prettyExpectedType) where

import HHC.Core.Syntax
import HHC.Core.Pretty

--   expr1 :: forall e a. e -> Maybe a -> Either e a
--   expr1 err x = case x of
--     Nothing -> Left err
--     Just y -> let z = 5 in case z of
--       5 -> Right y
--       w -> Right y
expr1 :: Expr
expr1 =
  TyLam (TypeVar "e") KStar $
    TyLam (TypeVar "a") KStar $
      Lam (TermVar "err") (TVar (TypeVar "e")) $
        Lam (TermVar "x") (TApp (TCon (TyCon "Maybe")) (TVar (TypeVar "a"))) $
          Case
            (Var (TermVar "x"))
            [ -- match Nothing
              Alt
                (PCon (DataCon "Nothing") [])
                (Con (DataCon "Left") [TVar (TypeVar "e"), TVar (TypeVar "a")] [Var (TermVar "err")]),
              -- match Just y
              Alt
                (PCon (DataCon "Just") [TermVar "y"])
                ( Let
                    (TermVar "z")
                    (Lit (LInt 5))
                    ( Case
                        (Var (TermVar "z"))
                        [ Alt (PLit (LInt 5)) (Con (DataCon "Right") [TVar (TypeVar "e"), TVar (TypeVar "a")] [Var (TermVar "y")]),
                          Alt (PVar (TermVar "w")) (Con (DataCon "Right") [TVar (TypeVar "e"), TVar (TypeVar "a")] [Var (TermVar "y")])
                        ]
                    )
                )
            ]

testExpr :: Expr
testExpr =
  Let (TermVar "f") expr1 $
    App
      ( App
          (TyApp (TyApp (Var (TermVar "f")) (TCon (TyCon "Bool"))) (TCon (TyCon "Int")))
          (Lit (LBool False))
      )
      (Con (DataCon "Just") [TCon (TyCon "Int")] [Lit (LInt 5)])

expectedType :: Type
expectedType =
  TApp
    (TApp (TCon (TyCon "Either")) (TCon (TyCon "Bool")))
    (TCon (TyCon "Int"))

prettyExpr1 :: IO ()
prettyExpr1 = putStrLn $ renderExpr expr1

prettyTestExpr :: IO ()
prettyTestExpr = putStrLn $ renderExpr testExpr

prettyExpectedType :: IO ()
prettyExpectedType = putStrLn $ renderType expectedType
