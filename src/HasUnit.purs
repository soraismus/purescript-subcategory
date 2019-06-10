module Control.Subcategory.HasUnit
  ( class HasUnit
  , terminate
  , terminate'
  , unit
  , unit'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Restrictable (restrict)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)
import Type.Proxy (Proxy3(Proxy3))

class
  ObjectOf c u
    <= HasUnit
      (c :: Type -> Type -> Type)
      (u :: Type)
      | c -> u
      where
      terminate' :: forall v. ObjectOf c u => ObjectOf c v => Proxy3 c -> c v u
      unit' :: ObjectOf c u => Proxy3 c -> u

terminate
  :: forall c u v
   . HasUnit c u
  => ObjectOf c u
  => ObjectOf c v
  => c v u
terminate = terminate' (Proxy3 :: Proxy3 c)

unit :: forall c u. HasUnit c u => ObjectOf c u => u
unit = unit' (Proxy3 :: Proxy3 c)

instance hasUnitFn :: HasUnit Function Unit where
  terminate' _ _ = Unit.unit
  unit' _ = Unit.unit

instance hasUnitBuilder :: HasUnit Builder (Record ()) where
  terminate' _ = restrict fn
    where
    fn :: forall r. ObjectOf Builder r => r -> Record ()
    fn r = {}
  unit' _ = {}
