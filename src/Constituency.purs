module Control.Subcategory.Constituency
  ( class ObjectOf
  , class OperatorOf
  ) where

import Record.Builder (Builder)

class ObjectOf (p :: Type -> Type -> Type) (a :: Type)

class OperatorOf (p :: Type -> Type -> Type) (a :: Type -> Type)

instance objectOfFn :: ObjectOf Function a
instance typeOperatorOfFn :: OperatorOf Function a

instance objectOfBuilderRecord :: ObjectOf Builder (Record r)
