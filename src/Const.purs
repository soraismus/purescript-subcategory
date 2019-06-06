module Control.Restricted.Const
  where

import Control.Restricted.Category (class Category)
import Control.Restricted.ObjectOf (class ObjectOf)
import Data.Function (compose, const) as Function
import Record.Builder (Builder)
import Unsafe.Coerce (unsafeCoerce)

class Category c <= Const (c :: Type -> Type -> Type) where
  const :: forall v0 v1. ObjectOf c v0 => ObjectOf c v1 => v0 -> c v1 v0

instance constFn :: Const Function where
  const = Function.const

instance constBuilder :: Const Builder where
  const = constBuilder'
    where
    mkBuilder
      :: forall v0 v1
       . ObjectOf Builder v0
      => ObjectOf Builder v0
      => (v1 -> v0)
      -> (Builder v1 v0)
    mkBuilder = unsafeCoerce
    constBuilder'
      :: forall v0 v1
       . ObjectOf Builder v0
      => ObjectOf Builder v1
      => v0
      -> Builder v1 v0
    constBuilder' = Function.compose mkBuilder Function.const
