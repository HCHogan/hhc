module HHC.Core.Pretty
  ( pprExpr,
    pprType,
    renderExpr,
    renderType,
  )
where

import HHC.Core.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))

-- | 渲染 ExprP 为 Doc
pprExpr :: ExprP String -> Doc
pprExpr expr = case expr of
  Var v ->
    pprTermVar v
  Lam v ty body ->
    -- λ(x:τ). body
    char 'λ'
      <> parens (pprTermVar v <> colon <> pprType ty)
      <> char '.'
      <> space
      <> pprExpr body
  App f x ->
    let fDoc = if isAtomicExpr f then pprExpr f else parens (pprExpr f)
        xDoc = if isAtomicExpr x then pprExpr x else parens (pprExpr x)
     in fDoc <+> xDoc
  TyLam tv _kind body ->
    -- Λ a. body
    char 'Λ' <+> pprTypeVar tv <> char '.' <+> pprExpr body
  TyApp e ty ->
    let eDoc = if isAtomicExpr e then pprExpr e else parens (pprExpr e)
     in eDoc <> char '@' <> pprType ty
  Let v rhs body ->
    text "let"
      <+> pprTermVar v
      <+> equals
      <+> pprExpr rhs
      <+> text " in "
      <> pprExpr body
  Con dc tys args ->
    -- DataCon @t1 @t2 a b
    let dcDoc = pprDataCon dc
        tyDocs = map (\t -> char '@' <> pprType t) tys
        argDocs = map (\a -> if isAtomicExpr a then pprExpr a else parens (pprExpr a)) args
     in hsep (dcDoc : tyDocs ++ argDocs)
  Case scrut alts ->
    text "case" <+> pprExpr scrut <+> text "of"
      $$ nest 2 (vcat (map pprAlt alts))
  Lit l ->
    pprLit l

-- | 渲染 AltP
pprAlt :: AltP String -> Doc
pprAlt (Alt pat rhs) =
  pprPat pat <+> text "->" <+> pprExpr rhs

-- | 渲染模式
pprPat :: PatP String -> Doc
pprPat = \case
  PVar v -> pprTermVar v
  PLit l -> pprLit l
  PCon dc vs ->
    pprDataCon dc <+> hsep (map pprTermVar vs)

-- | 渲染类型
pprType :: TypeP String -> Doc
pprType tp = case tp of
  TVar v ->
    pprTypeVar v
  TCon c ->
    pprTyCon c
  TArr t1 t2 ->
    let t1d = if isArrowType t1 then parens (pprType t1) else pprType t1
     in t1d <+> text "->" <+> pprType t2
  TApp t1 t2 ->
    let d1 = if isAtomicType t1 then pprType t1 else parens (pprType t1)
        d2 = if isAtomicType t2 then pprType t2 else parens (pprType t2)
     in d1 <+> d2
  TForall v _kind t ->
    -- forall (a :: κ). τ
    text "forall"
      <+> parens (pprTypeVar v <+> text "::" <+> pprKind _kind)
      <> text "."
      <+> pprType t

-- | 渲染 Kind
pprKind :: Kind -> Doc
pprKind = \case
  KStar -> char '*'
  KFun a b ->
    let da = if isKFun a then parens (pprKind a) else pprKind a
     in da <+> text "->" <+> pprKind b

-- | 渲染 Literal
pprLit :: Literal -> Doc
pprLit = \case
  LInt i -> integer i
  LChar c -> text (show c)
  LString s -> text (show s)
  LBool b -> text (if b then "True" else "False")

-- | 基本名称打印
pprTermVar :: TermVarP String -> Doc
pprTermVar (TermVar s) = text s

pprTypeVar :: TypeVarP String -> Doc
pprTypeVar (TypeVar s) = text s

pprTyCon :: TyConP String -> Doc
pprTyCon (TyCon s) = text s

pprDataCon :: DataConP String -> Doc
pprDataCon (DataCon s) = text s

-- | 判断是否需要加括号
isAtomicExpr :: ExprP a -> Bool
isAtomicExpr = \case
  Var {} -> True
  Lit {} -> True
  Con {} -> True
  _ -> False

isAtomicType :: TypeP a -> Bool
isAtomicType = \case
  TVar {} -> True
  TCon {} -> True
  _ -> False

isArrowType :: TypeP a -> Bool
isArrowType = \case
  TArr {} -> True
  _ -> False

isKFun :: Kind -> Bool
isKFun = \case
  KFun {} -> True
  _ -> False

-- | 方便渲染为 String
renderExpr :: ExprP String -> String
renderExpr = render . pprExpr

renderType :: TypeP String -> String
renderType = render . pprType
