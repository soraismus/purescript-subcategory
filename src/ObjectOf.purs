module Control.Restricted.ObjectOf
  ( class ObjectOf
  , class TypeOperatorOf
  ) where

import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Void (Void)
import Record.Builder (Builder)

class ObjectOf (p :: Type -> Type -> Type) (a :: Type)

class TypeOperatorOf (p :: Type -> Type -> Type) (a :: Type -> Type)

instance objectOfFn :: ObjectOf Function a
instance typeOperatorOfFn :: TypeOperatorOf Function a

instance objectOfBuilderVoid :: ObjectOf Builder Void
instance objectOfBuilderRecord :: ObjectOf Builder (Record r)
else instance objectOfBuilderTypeOperator
  :: ( ObjectOf Builder a
     , TypeOperatorOf Builder f
     )
  => ObjectOf Builder (f a)
instance typeOperatorOfBuilderBuilder
  :: ObjectOf Builder a
  => TypeOperatorOf Builder (Builder a)
instance typeOperatorOfBuilderEither
  :: ObjectOf Builder a
  => TypeOperatorOf Builder (Either a)
instance typeOperatorOfBuilderTuple
  :: ObjectOf Builder a
  => TypeOperatorOf Builder (Tuple a)
