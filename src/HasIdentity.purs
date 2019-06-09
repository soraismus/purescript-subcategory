module Control.Subcategory.HasIdentity
  ( class HasIdentity
  , identity
  ) where

import Control.Category (identity) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Record.Builder (Builder)

class HasIdentity
  (c :: Type -> Type -> Type)
  (p :: Type -> Type -> Type)
  where
  identity :: forall a. ObjectOf c a => p a a

instance hasIdentityFn :: HasIdentity Function Function where
  identity = Unrestricted.identity

instance hasIdentityBuilder :: HasIdentity Builder Builder where
  identity = Unrestricted.identity
