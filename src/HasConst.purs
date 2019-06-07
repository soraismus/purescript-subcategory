module Control.Restricted.HasConst
  ( class HasConst
  , const
  ) where

import Control.Restricted.ObjectOf (class ObjectOf)
import Data.Function ((<<<))
import Data.Function (const) as Function
import Record.Builder (Builder)
import Unsafe.Coerce (unsafeCoerce)

class HasConst (c :: Type -> Type -> Type) where
--   const :: forall v0 v1. ObjectOf c v0 => ObjectOf c v1 => v0 -> c v1 v0
  const :: forall v0 v1. ObjectOf c v0 => ObjectOf c v1 => c v0 (c v1 v0)

instance constFn :: HasConst Function where
  const = Function.const

-- instance constBuilder :: HasConst Builder where
--   const = mkBuilder <<< Function.const
--     where
--     mkBuilder
--       :: forall v0 v1
--        . ObjectOf Builder v0
--       => ObjectOf Builder v1
--       => (v1 -> v0)
--       -> (Builder v1 v0)
--     mkBuilder = unsafeCoerce
