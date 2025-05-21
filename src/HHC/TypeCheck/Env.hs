module HHC.TypeCheck.Env (
  TypeEnv(..),
  extend,
  remove,
  emptyTypeEnv,
  lookup,
  merge,
  mergeEnvs,
  singleton,
  keys,
  fromList,
  toList,
) where

import Prelude hiding (lookup)

import HHC.Syntax
import HHC.Types

import Data.Map qualified as M

newtype TypeEnv = TypeEnv (M.Map Var Scheme)
  deriving (Semigroup, Monoid)

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv e) (x, s) = TypeEnv $ M.insert x s e

remove :: TypeEnv -> Var -> TypeEnv
remove (TypeEnv e) v = TypeEnv $ M.delete v e

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv M.empty

lookup :: Var -> TypeEnv -> Maybe Scheme
lookup key (TypeEnv tys) = M.lookup key tys

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge (TypeEnv a) (TypeEnv b) = TypeEnv (M.union a b)

mergeEnvs :: [TypeEnv] -> TypeEnv
mergeEnvs = foldl' merge emptyTypeEnv

singleton :: Var -> Scheme -> TypeEnv
singleton x y = TypeEnv (M.singleton x y)

keys :: TypeEnv -> [Var]
keys (TypeEnv env) = M.keys env

fromList :: [(Var, Scheme)] -> TypeEnv
fromList xs = TypeEnv (M.fromList xs)

toList :: TypeEnv -> [(Var, Scheme)]
toList (TypeEnv env) = M.toList env

