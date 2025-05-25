module HHC.Core.Pretty
  ( renderExpr,
    renderType,
  )
where

import HHC.Core.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

maybeParens :: Bool -> Doc -> Doc
maybeParens True doc = parens doc
maybeParens False doc = doc

letPrec, lamPrec, casePrec :: Int
letPrec = 0
lamPrec = 0
casePrec = 0

appPrec :: Int
appPrec = 10

tyAppPrec :: Int
tyAppPrec = 11

atomPrec :: Int
atomPrec = 12

forallPrec :: Int
forallPrec = 0

arrPrec :: Int
arrPrec = 1

class Pretty a where
  ppr :: a -> Doc
  pprPrec :: Int -> a -> Doc
  pprPrec _ = ppr

instance Pretty String where
  ppr = text
  pprPrec _ = text

instance Pretty (TermVarP String) where
  ppr (TermVar s) = text s
  pprPrec _ (TermVar s) = text s

instance Pretty (TypeVarP String) where
  ppr (TypeVar s) = text s
  pprPrec _ (TypeVar s) = text s

instance Pretty (TyConP String) where
  ppr (TyCon s) = text s
  pprPrec _ (TyCon s) = text s

instance Pretty (DataConP String) where
  ppr (DataCon s) = text s
  pprPrec _ (DataCon s) = text s

instance Pretty Kind where
  ppr = pprKindPrec 0
  pprPrec = pprKindPrec

pprKindPrec :: Int -> Kind -> Doc
pprKindPrec _ KStar = text "*"
pprKindPrec d (KFun k1 k2) =
  maybeParens (d > arrPrec) $
    pprKindPrec (arrPrec + 1) k1 <+> text "→" <+> pprKindPrec arrPrec k2

instance Pretty (TypeP String) where
  ppr = pprTypePrec 0
  pprPrec = pprTypePrec

pprTypePrec :: Int -> TypeP String -> Doc
pprTypePrec _ (TVar v) = ppr v
pprTypePrec _ (TCon tc) = ppr tc
pprTypePrec d (TArr t1 t2) =
  maybeParens (d > arrPrec) $
    pprTypePrec (arrPrec + 1) t1 <+> text "→" <+> pprTypePrec arrPrec t2
pprTypePrec d (TForall tv k ty) =
  maybeParens (d > forallPrec) $
    hang
      (text "∀(" <> ppr tv <+> text "::" <+> pprKindPrec 0 k <> text ").") -- Changed : to :: and added spaces
      2
      (pprTypePrec forallPrec ty)
pprTypePrec d (TApp t1 t2) =
  maybeParens (d > appPrec) $
    pprTypePrec appPrec t1 <+> pprTypePrec (appPrec + 1) t2

instance Pretty Literal where
  ppr (LInt i) = integer i
  ppr (LChar c) = quotes (char c)
  ppr (LString s) = doubleQuotes (text s)
  ppr (LBool True) = text "True"
  ppr (LBool False) = text "False"

instance Pretty (PatP String) where
  ppr (PVar v) = ppr v
  ppr (PLit l) = ppr l
  ppr (PCon dc vs) = hsep (ppr dc : map ppr vs)

instance Pretty (AltP String) where
  ppr (Alt pat expr) =
    ppr pat <+> text "->" <+> pprPrec 0 expr

instance Pretty (ExprP String) where
  ppr = pprPrec 0
  pprPrec = pprExprPrec

pprExprPrec :: Int -> ExprP String -> Doc
pprExprPrec _ (Var v) = ppr v
pprExprPrec _ (Lit l) = ppr l
pprExprPrec d (Lam x ty body) =
  maybeParens (d > lamPrec) $
    text "λ(" <> ppr x <+> text "::" <+> pprTypePrec 0 ty <> text ")." <+> pprExprPrec lamPrec body
pprExprPrec d (App e1 e2) =
  maybeParens (d > appPrec) $
    pprExprPrec appPrec e1 <+> pprExprPrec (appPrec + 1) e2
pprExprPrec d (TyLam tv k body) =
  maybeParens (d > lamPrec) $
    text "Λ(" <> ppr tv <+> text "::" <+> pprKindPrec 0 k <> text ")." <+> pprExprPrec lamPrec body
pprExprPrec d (TyApp e ty) =
  maybeParens (d > tyAppPrec) $
    pprExprPrec tyAppPrec e <+> char '@' <> pprTypePrec atomPrec ty
pprExprPrec d (Let x e1 e2) =
  maybeParens (d > letPrec) $
    text "let" <+> ppr x <+> equals <+> pprExprPrec 0 e1 <+> text "in" <+> pprExprPrec letPrec e2
pprExprPrec _ (Con dc tyArgs exArgs) =
  let dcDoc = ppr dc
      tyDocs = map (\ty -> char '@' <> pprTypePrec atomPrec ty) tyArgs
      exDocs = map (pprExprPrec (appPrec + 1)) exArgs
   in hsep (dcDoc : tyDocs ++ exDocs)
pprExprPrec d (Case scrut alts) =
  maybeParens (d > casePrec) $
    hang
      (text "case" <+> pprExprPrec 0 scrut <+> text "of")
      2
      (vcat (map ppr alts))

renderExpr :: Expr -> String
renderExpr expr = render (ppr expr)

renderType :: Type -> String
renderType ty = render (ppr ty)
