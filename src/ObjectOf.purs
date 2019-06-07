module Control.Restricted.ObjectOf
  ( class ObjectOf
  , class OperatorOf
  ) where

import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Void (Void)
import Record.Builder (Builder)

class ObjectOf (p :: Type -> Type -> Type) (a :: Type)

class OperatorOf (p :: Type -> Type -> Type) (a :: Type -> Type)

instance objectOfFn :: ObjectOf Function a
instance typeOperatorOfFn :: OperatorOf Function a

-- instance objectOfBuilderVoid :: ObjectOf Builder Void
instance objectOfBuilderRecord :: ObjectOf Builder (Record r)
-- else instance objectOfBuilderTypeOperator
--   :: ( ObjectOf Builder a
--      , OperatorOf Builder f
--      )
--   => ObjectOf Builder (f a)
-- instance typeOperatorOfBuilderBuilder
--   :: ObjectOf Builder a
--   => OperatorOf Builder (Builder a)
-- instance typeOperatorOfBuilderEither
--   :: ObjectOf Builder a
--   => OperatorOf Builder (Either a)
-- instance typeOperatorOfBuilderTuple
--   :: ObjectOf Builder a
--   => OperatorOf Builder (Tuple a)
