module HHC.Core.Tc where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import Data.Map qualified as Map
import HHC.Core.Syntax

-- | Typing environment includes term vars, type vars (kinds), and data constructors
data Env = Env
  { termEnv :: Map TermVar Type,
    typeEnv :: Map TypeVar Kind,
    tyConEnv :: Map TyCon Kind,
    conEnv :: Map DataCon ([TypeVar], [Type], Type)
  }

-- | Empty initial environment
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Map.empty

-- | Type-checking errors
data TcError
  = UnboundVariable TermVar
  | UnboundConstructor DataCon
  | UnboundTypeVariable TypeVar
  | ConstructorArityMismatch DataCon Int Int
  | ConstructorArgArityMismatch DataCon Int Int
  | TypeMismatch Type Type
  | NotFunction Type
  | NotPolymorphic Type
  | UnboundTypeConstructor TyCon
  | KindMismatch Kind Kind
  | OccursCheckFailed TypeVar Type
  deriving (Eq, Show)

-- | Tc monad stacking Reader for Env and Except for errors
newtype Tc a = Tc {runTc :: ReaderT Env (Except TcError) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError TcError)

-- | Execute a type-check action
execTc :: Env -> Tc a -> Either TcError a
execTc env m = runExcept (runReaderT (runTc m) env)

-- | Look up a term variable
lookupVar :: TermVar -> Tc Type
lookupVar x = do
  env <- asks termEnv
  case Map.lookup x env of
    Just t -> return t
    Nothing -> throwError (UnboundVariable x)

-- | Look up a data constructor
lookupCon :: DataCon -> Tc ([TypeVar], [Type], Type)
lookupCon dc = do
  env <- asks conEnv
  case Map.lookup dc env of
    Just info -> return info
    Nothing -> throwError (UnboundConstructor dc)

-- | Ensure a type has the expected kind
checkKind :: Kind -> Type -> Tc ()
checkKind expected ty = do
  k <- inferKind ty
  unless (k == expected) (throwError (KindMismatch expected k))

-- | Infer the kind of a type, consulting typeEnv and tyConEnv
inferKind :: Type -> Tc Kind
inferKind = \case
  TVar v -> do
    ctx <- asks typeEnv
    case Map.lookup v ctx of
      Just k -> return k
      Nothing -> throwError (UnboundTypeVariable v)
  TCon tc -> do
    ctx <- asks tyConEnv
    case Map.lookup tc ctx of
      Just k -> return k
      Nothing -> throwError (UnboundTypeConstructor tc)
  TArr t1 t2 -> do
    checkKind KStar t1
    checkKind KStar t2
    return KStar
  TApp tF tX -> do
    kF <- inferKind tF
    case kF of
      KFun kArg kRes -> do
        checkKind kArg tX
        return kRes
      _ -> throwError (KindMismatch (KFun KStar KStar) kF)
  TForall v k body -> do
    kBody <-
      local
        (\env -> env {typeEnv = Map.insert v k (typeEnv env)})
        (inferKind body)
    unless (kBody == KStar) (throwError (KindMismatch KStar kBody))
    return KStar

-- | Substitutions for type variables
type Subst = Map TypeVar Type

-- | Occurs-check: avoid infinite types
occurs :: TypeVar -> Type -> Bool
occurs v = \case
  TVar v' -> v == v'
  TArr l r -> occurs v l || occurs v r
  TApp f x -> occurs v f || occurs v x
  TForall v' _ b -> v /= v' && occurs v b
  TCon _ -> False

-- | Unify two types, producing a substitution
unify :: Type -> Type -> Tc Subst
unify t1 t2 = case (t1, t2) of
  (TVar a, t)
    | t == TVar a -> return Map.empty
    | occurs a t -> throwError (OccursCheckFailed a t)
    | otherwise -> return (Map.singleton a t)
  (t, TVar a)
    | t == TVar a -> return Map.empty
    | occurs a t -> throwError (OccursCheckFailed a (TVar a))
    | otherwise -> return (Map.singleton a t)
  (TArr l1 r1, TArr l2 r2) -> do
    s1 <- unify l1 l2
    s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
    return (s2 `Map.union` s1)
  (TApp f1 x1, TApp f2 x2) -> do
    s1 <- unify f1 f2
    s2 <- unify (applySubst s1 x1) (applySubst s1 x2)
    return (s2 `Map.union` s1)
  (t1', t2')
    | t1' == t2' -> return Map.empty
    | otherwise -> throwError (TypeMismatch t1' t2')

-- | Apply a substitution to a type
applySubst :: Subst -> Type -> Type
applySubst subst = \case
  TVar a -> Map.findWithDefault (TVar a) a subst
  TArr l r -> TArr (applySubst subst l) (applySubst subst r)
  TForall a k body ->
    TForall a k (applySubst (Map.delete a subst) body)
  TApp f x -> TApp (applySubst subst f) (applySubst subst x)
  TCon c -> TCon c

-- | Type of a literal
literalType :: Literal -> Type
literalType = \case
  LInt _ -> TCon (TyCon "Int")
  LChar _ -> TCon (TyCon "Char")
  LString _ -> TCon (TyCon "String")
  LBool _ -> TCon (TyCon "Bool")

-- | Pattern checking: returns bindings for term variables
typeCheckPat :: Pat -> Type -> Tc [(TermVar, Type)]
typeCheckPat pat ty = case pat of
  PVar x -> return [(x, ty)]
  PLit l -> do
    let lt = literalType l
    unless (lt == ty) (throwError (TypeMismatch lt ty))
    return []
  PCon dc vars -> do
    (tvs, paramTs, resTy) <- lookupCon dc
    -- 1) bind type variables
    subst0 <- unify resTy ty
    let instTs = map (applySubst subst0) paramTs
    -- 2) check datacon argument arity
    let expArgs = length instTs
        actArgs = length vars
    when (expArgs /= actArgs) $
      throwError (ConstructorArgArityMismatch dc expArgs actArgs)
    return (zip vars instTs)

-- | Main type-check function
typeCheck :: Expr -> Tc Type
typeCheck = \case
  Var x -> lookupVar x
  Lit l -> return (literalType l)
  Lam x ty body -> do
    checkKind KStar ty
    retTy <-
      local
        (\e -> e {termEnv = Map.insert x ty (termEnv e)})
        (typeCheck body)
    return (TArr ty retTy)
  App e1 e2 -> do
    t1 <- typeCheck e1
    t2 <- typeCheck e2
    case t1 of
      TArr arg ret -> do
        s <- unify arg t2
        return (applySubst s ret)
      _ -> throwError (NotFunction t1)
  TyLam a k e -> do
    retTy <-
      local
        (\e0 -> e0 {typeEnv = Map.insert a k (typeEnv e0)})
        (typeCheck e)
    return (TForall a k retTy)
  TyApp e tyArg -> do
    tFun <- typeCheck e
    case tFun of
      TForall a k body -> do
        checkKind k tyArg
        return (applySubst (Map.singleton a tyArg) body)
      _ -> throwError (NotPolymorphic tFun)
  Let x e1 e2 -> do
    t1 <- typeCheck e1
    local
      (\e -> e {termEnv = Map.insert x t1 (termEnv e)})
      (typeCheck e2)
  Con dc tys es -> do
    (tvs, paramTs, resTy) <- lookupCon dc
    -- 类型参数个数检查
    let expTyArgs = length tvs
        actTyArgs = length tys
    when (expTyArgs /= actTyArgs) $
      throwError (ConstructorArityMismatch dc expTyArgs actTyArgs)
    -- 检查每个类型参数的 kind
    forM_ tys (checkKind KStar)
    -- 专门化 result type
    let subst0 = Map.fromList (zip tvs tys)
    -- term 参数个数检查
    let expTmArgs = length paramTs
        actTmArgs = length es
    when (expTmArgs /= actTmArgs) $
      throwError (ConstructorArgArityMismatch dc expTmArgs actTmArgs)
    -- 检查每个参数类型
    zipWithM_
      ( \argExpr expectedTy -> do
          t <- typeCheck argExpr
          _ <- unify expectedTy t
          return ()
      )
      es
      (map (applySubst subst0) paramTs)
    return (applySubst subst0 resTy)
  Case scrut alts -> do
    scrTy <- typeCheck scrut
    altTys <- forM alts $ \(Alt pat rhs) -> do
      binds <- typeCheckPat pat scrTy
      local
        (\e -> e {termEnv = termEnv e `Map.union` Map.fromList binds})
        (typeCheck rhs)
    case altTys of
      t : ts | all (== t) ts -> return t
      _ -> throwError (TypeMismatch (head altTys) (last altTys))
