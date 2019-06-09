module Control.Subcategory.HasIdentity
  ( class HasIdentity
  , identity
  ) where

import Control.Category (identity) as Unrestricted
import Record.Builder (Builder)

class HasIdentity (c :: Type -> Type -> Type) where
  identity :: forall a. c a a

instance hasIdentityFn :: HasIdentity Function where
  identity = Unrestricted.identity

instance hasIdentityBuilder :: HasIdentity Builder where
  identity = Unrestricted.identity

-- instance hasIdentityCategory
--   :: Category c
--   => HasIdentity c
--   where
--   identity = Unrestricted.identity
-- else instance hasIdentityRestrictable
--   :: Restrictable Function c
--   => HasIdentity c
--   where
--   identity = restrict Unrestricted.identity
