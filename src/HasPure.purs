module Control.Restricted.HasPure ( class HasPure
  , pure
  , liftA1
--   , unless, when
  ) where

import Control.Applicative (class Applicative, pure) as Unrestricted
import Control.Restricted.HasApply (class HasApply, apply, (<*>))
import Control.Restricted.HasConst (class HasConst, const)
import Control.Restricted.HasEval (class HasEval, eval)
import Control.Restricted.HasIdentity (class HasIdentity, identity)
import Control.Restricted.HasMap (class HasMap, map, (<$>))
import Control.Restricted.HasUnit (class HasUnit, unit)
import Control.Restricted.ObjectOf (class ObjectOf)
import Data.Unit (Unit)
import Data.Unit (unit) as Unit

class HasPure c f where
  pure :: forall v. ObjectOf c v => v -> f v

type DictHasPure c f =
  { pure :: forall v. HasPure c f => ObjectOf c v => v -> f v }

type DictHasUnit c u =
  { unit :: HasUnit c u => ObjectOf c u => Unit -> u }

liftA1
  :: forall c f v0 v1
   . HasApply c f
  => HasPure c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v0 v1)
  => DictHasPure c f
  -> c v0 v1
  -> f v0
  -> f v1
liftA1 { pure } f x = pure f <*> x

when
  :: forall c m u
   . HasPure c m
  => HasUnit c u
  => ObjectOf c u
  => DictHasPure c m
  -> Boolean
  -> m u
  -> m u
when _ true m = m
when { pure } false _ = pure (dictHasUnit.unit Unit.unit)
  where
  dictHasUnit :: DictHasUnit c u
  dictHasUnit = { unit: \_ -> unit }

-- -- | Perform an applicative action unless a condition is true.
-- unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
-- unless false m = m
-- unless true _ = pure unit

instance hasPureUnrestricted
  :: Unrestricted.Applicative f
  => HasPure Function f
  where
  pure = Unrestricted.pure
