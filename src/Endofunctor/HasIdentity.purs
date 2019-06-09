module Control.Subcategory.HasIdentity
  ( class HasIdentity
  , identity
  ) where

import Control.Category (identity) as Unrestricted
import Record.Builder (Builder)

-- | The type `p` is a relation that satisfies reflexivity.
class HasIdentity (p :: Type -> Type -> Type) where
  identity :: forall a. p a a

instance hasIdentityFn :: HasIdentity Function where
  identity = Unrestricted.identity

instance hasIdentityBuilder :: HasIdentity Builder where
  identity = Unrestricted.identity
