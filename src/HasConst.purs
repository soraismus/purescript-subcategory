module Control.Subcategory.HasConst
  ( class HasConst
  , const
  ) where

import Control.Subcategory.ObjectOf (class ObjectOf)
import Data.Function (const) as Function
import Record.Builder (Builder)
import Unsafe.Coerce (unsafeCoerce)

class HasConst (c :: Type -> Type -> Type) where
  const :: forall v0 v1. ObjectOf c v0 => ObjectOf c v1 => c v0 (c v1 v0)

instance constFn :: HasConst Function where
  const = Function.const

instance constBuilder :: HasConst Builder where
  const = mkBuilder \v0 -> mkBuilder (const v0)
    where
    mkBuilder
      :: forall v0 v1
       . ObjectOf Builder v0
      => ObjectOf Builder v1
      => (v0 -> v1)
      -> (Builder v0 v1)
    mkBuilder = unsafeCoerce
