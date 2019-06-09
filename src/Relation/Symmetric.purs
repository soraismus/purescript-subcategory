module Relation.Symmetric
  ( class Symmetric
  , swap
  ) where

class Symmetric (p :: Type -> Type -> Type) where
  swap :: forall v0 v1. p v0 v1 -> p v1 v0
