module Relation.Reflexive
  ( class Reflexive
  , identity
  ) where

class Reflexive (p :: Type -> Type -> Type) where
  identity :: forall a. p a a
