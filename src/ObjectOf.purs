module Control.Subcategory.ObjectOf
  ( class ObjectOf
  ) where

import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Void (Void)
import Record.Builder (Builder)

class ObjectOf (p :: Type -> Type -> Type) (a :: Type)

instance objectOfFn :: ObjectOf Function a

instance objectOfBuilderRecord :: ObjectOf Builder (Record r)
instance objectOfBuilderVoid :: ObjectOf Builder Void
instance objectOfBuilderBuilder
  :: ( ObjectOf Builder a
     , ObjectOf Builder b
     )
  => ObjectOf Builder (Builder a b)
instance objectOfBuilderTuple
  :: ( ObjectOf Builder a
     , ObjectOf Builder b
     )
  => ObjectOf Builder (Tuple a b)
instance objectOfBuilderEither
  :: ( ObjectOf Builder a
     , ObjectOf Builder b
     )
  => ObjectOf Builder (Either a b)

-- type LeibnizObject = ...
-- instance objectOfLeibniz
--   :: TypeEquals a LeibnizObject
--   => ObjectOf Leibniz LeibnizObject
