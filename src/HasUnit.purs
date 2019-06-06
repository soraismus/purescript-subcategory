module Control.Restricted.HasUnit
  ( class HasUnit
  , unit
  ) where

import Control.Restricted.ObjectOf (class ObjectOf)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)

class HasUnit
  (c :: Type -> Type -> Type)
  (u :: Type)
  | c -> u
  where
  unit :: ObjectOf c u => u

instance hasUnitFn :: HasUnit Function Unit where
  unit = Unit.unit

instance hasUnitBuilder :: HasUnit Builder (Record ()) where
  unit = {}
