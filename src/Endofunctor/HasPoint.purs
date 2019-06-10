module Control.Subcategory.Endofunctor.HasPoint
  ( class HasPoint
  , point
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Restrictable (restrict)
import Data.Unit (Unit)
import Record.Builder (Builder)

class
  ObjectOf c u
    <= HasPoint
      (c :: Type -> Type -> Type)
      (u :: Type)
      | c -> u  -- This fundep is to accommodate the category `Builder`.
      where
      point :: forall v. ObjectOf c v => v -> c u v

instance hasPointFn :: HasPoint Function Unit where
  point v _ = v

instance hasPointBuilder :: HasPoint Builder (Record ()) where
  point record = restrict (\_ -> record)
