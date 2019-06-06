module Control.Restricted.Eval
  ( class Eval
  , eval
  ) where

import Control.Restricted.Closed (class Closed)
import Control.Restricted.ObjectOf (class ObjectOf)
import Data.Function (apply) as Function
import Record.Builder (Builder)
import Record.Builder (build) as Builder
import Unsafe.Coerce (unsafeCoerce)

class Closed c <= Eval (c :: Type -> Type -> Type) where
  eval
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v0 v1)
    => (c v0 v1)
    -> v0
    -> v1

instance closedMonoidalFn :: Eval Function where
  eval = Function.apply

instance closedMonoidalBuilder :: Eval Builder where
  eval builder record = coerceBuild Builder.build builder record
    where
    coerceBuild
      :: (forall r1 r2
             . Builder (Record r1) (Record r2)
            -> Record r1
            -> Record r2)
      -> (forall a b
             . ObjectOf Builder a
            => ObjectOf Builder b
            => Builder a b
            -> a
            -> b)
    coerceBuild = unsafeCoerce
