module Control.Subcategory.Endofunctor.HasPoint
  ( class HasPoint
  , point
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Slackable (slacken)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)
import Unsafe.Coerce (unsafeCoerce)

class
  ObjectOf c u
    <= HasPoint
      (c :: Type -> Type -> Type)
      (u :: Type)
      | c -> u
      where
      point
        :: forall v
         . ObjectOf c u
        => ObjectOf c v
        => v
        -> c u v

instance hasPointFn :: HasPoint Function Unit where
  point v _ = v

instance hasPointBuilder :: HasPoint Builder (Record ()) where
  point record = builder
    where
    point' :: forall r. ObjectOf Builder r => r -> Record () -> r
    point' r _ = r
    builder :: forall r. ObjectOf Builder r => Builder (Record ()) r
    builder = unsafeCoerce (point' record)


f :: c u v1 -> c v0 v1
f
  :: HasDimap c c
