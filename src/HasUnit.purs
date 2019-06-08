module Control.Subcategory.HasUnit
  ( class HasUnit
  , unit
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)
import Type.Proxy (Proxy3)

class HasUnit
  (c :: Type -> Type -> Type)
  (u :: Type)
  | c -> u
  where
  unit :: ObjectOf c u => Proxy3 c -> u

instance hasUnitFn :: HasUnit Function Unit where
  unit _ = Unit.unit

instance hasUnitBuilder :: HasUnit Builder (Record ()) where
  unit _ = {}
