module Control.Subcategory.Endofunctor.HasUnpoint
  ( class HasUnpoint
  , unpoint
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Slackable (slacken)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit
import Record.Builder (Builder)
import Unsafe.Coerce (unsafeCoerce)

class
  ObjectOf c u
    <= HasUnpoint
      (c :: Type -> Type -> Type)
      (u :: Type)
      | c -> u
      where
      unpoint
        :: forall v
         . ObjectOf c u
        => ObjectOf c v
        => v
        -> c u v

instance hasUnpointFn :: HasUnpoint Function Unit where
  unpoint v _ = v

instance hasUnpointBuilder :: HasUnpoint Builder (Record ()) where
  unpoint record = builder
    where
    unpoint' :: forall r. ObjectOf Builder r => r -> Record () -> r
    unpoint' r _ = r
    builder :: forall r. ObjectOf Builder r => Builder (Record ()) r
    builder = unsafeCoerce (unpoint' record)
