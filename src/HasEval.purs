module Control.Subcategory.Slackable
  ( class Slackable
  , slacken
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Data.Function (apply) as Function
import Record.Builder (Builder)
import Record.Builder (build) as Builder
import Unsafe.Coerce (unsafeCoerce)

class Slackable (c :: Type -> Type -> Type) where
  slacken
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v0 v1)
    => (c v0 v1)
    -> v0
    -> v1

instance closedMonoidalFn :: Slackable Function where
  slacken = Function.apply

instance closedMonoidalBuilder :: Slackable Builder where
  slacken builder record = coerceBuild Builder.build builder record
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
