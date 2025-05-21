module HHC.TypeCheck.Error
  ( TypeError (..),
  )
where

import HHC.Types

data TypeError
  = InfiniteType TVar Type
  | UnificationFail Type Type
  | UnificationMismatch [Type] [Type]
  | UnboundVariable String
  deriving (Eq)
