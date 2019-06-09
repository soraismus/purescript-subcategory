module Control.Subcategory.Endofunctor.HasPoint
  ( class HasPoint
  , point
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Slackable (slacken)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)

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
        => c u v
        -> v

instance hasPointFn :: HasPoint Function Unit where
  point f = f Unit.unit

instance hasPointBuilder :: HasPoint Builder (Record ()) where
  point builder = slacken builder {}
