module Main (main) where

import Data.Map qualified as Map
import HHC.Core.Optimizer
import HHC.Core.Syntax
import HHC.Core.Tc
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "HHC"
    [ testGroup
        "HHC.Core.Tc"
        [ testCase "literal type Bool" $
            execTc exampleEnv (typeCheck (Lit (LBool True)))
              @?= Right (TCon (TyCon "Bool")),
          testCase "simple lambda" $
            let expr = Lam (TermVar "x") (TCon (TyCon "Int")) (Var (TermVar "x"))
             in execTc exampleEnv (typeCheck expr)
                  @?= Right (TArr (TCon (TyCon "Int")) (TCon (TyCon "Int"))),
          testCase "testExpr → Either Bool Int" $
            execTc exampleEnv (typeCheck testExpr)
              @?= Right expectedType
        ],
      testGroup
        "HHC.Core.Optimizer"
        [ testCase "constant folding: 1 + 2 → 3" $
            let expr = App (App (Var (TermVar "+")) (Lit (LInt 1))) (Lit (LInt 2))
             in optimizeExpr expr @?= Lit (LInt 3),
          testCase "beta-reduce nested let" $
            let expr2 =
                  Let
                    (TermVar "x")
                    (Lit (LInt 5))
                    ( App
                        (Lam (TermVar "y") (TCon (TyCon "Int")) (Var (TermVar "y")))
                        (Var (TermVar "x"))
                    )
             in optimizeExpr expr2 @?= Lit (LInt 5)
        ]
    ]

exampleEnv :: Env
exampleEnv =
  Env
    { termEnv = Map.empty,
      typeEnv = Map.empty,
      tyConEnv =
        Map.fromList
          [ (TyCon "Maybe", KFun KStar KStar),
            (TyCon "Either", KFun KStar (KFun KStar KStar)),
            (TyCon "Bool", KStar),
            (TyCon "Int", KStar),
            (TyCon "String", KStar),
            (TyCon "Char", KStar)
          ],
      conEnv =
        Map.fromList
          [ (DataCon "Nothing", ([TypeVar "a"], [], TApp (TCon (TyCon "Maybe")) (TVar (TypeVar "a")))),
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

-- >>> let expr = App (App (Var (TermVar "+")) (Lit (LInt 1))) (Lit (LInt 2))
-- >>> optimizeExpr expr
-- Lit (LInt 3)

-- >>> -- Beta-reduce nested lets
-- >>> let expr2 = Let (TermVar "x") (Lit (LInt 5)) (App (Lam (TermVar "y") (TCon (TyCon "Int")) (Var (TermVar "y"))) (Var (TermVar "x")))
-- >>> optimizeExpr expr2
-- Lit (LInt 5)
