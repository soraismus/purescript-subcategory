module Control.Subcategory.Identity
  ( class Identity
  , identity
  ) where

import Control.Category (identity) as Super
import Control.Subcategory.ObjectOf (class ObjectOf)
import Record.Builder (Builder)

class Identity (p :: Type -> Type -> Type) where
  identity :: forall a. ObjectOf p a => p a a

instance identityFn :: Identity Function where
  identity = Super.identity

instance identityBuilder :: Identity Builder where
  identity = Super.identity
