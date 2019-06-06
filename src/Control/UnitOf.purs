module Control.UnitOf
  ( class UnitOf
  , assertObjectOf
  , unit_
  ) where

import Prelude (unit)

import Control.ObjectOf (class ObjectOf)
import Record.Builder (Builder)

class UnitOf (a :: Type -> Type -> Type) (b :: Type) where
  assertObjectOf :: ObjectOf a b => Unit
  unit_ :: b

instance unitOfFn :: UnitOf Function Unit where
  assertObjectOf = unit
  unit_ = unit

instance unitOfBuilder :: UnitOf Builder (Record ()) where
  assertObjectOf = unit
  unit_ = {}
