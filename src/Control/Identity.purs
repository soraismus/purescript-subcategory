module Control.Identity_
  ( class Identity_
  , identity_
  ) where

import Control.Category (identity)
import Control.ObjectOf (class ObjectOf)
import Record.Builder (Builder)

class Identity_ (p :: Type -> Type -> Type) where
  identity_ :: forall a. ObjectOf p a => p a a

instance identity_Fn :: Identity_ Function where
  identity_ = identity

instance identity_Builder :: Identity_ Builder where
  identity_ = identity
