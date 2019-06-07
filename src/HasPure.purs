module Control.Restricted.HasPure
  ( class HasPure
  , liftA1
  , pure
  , unless
  , when
  ) where

import Control.Applicative (class Applicative, pure) as Unrestricted
import Control.Restricted.HasApply (class HasApply, (<*>))
import Control.Restricted.HasUnit (class HasUnit)
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

unless
  :: forall c m u
   . HasPure c m
  => HasUnit c u
  => ObjectOf c u
  => DictHasPure c m
  -> DictHasUnit c u
  -> Boolean
  -> m u
  -> m u
unless _        _        false m = m
unless { pure } { unit } true  _ = pure (unit Unit.unit)

when
  :: forall c m u
   . HasPure c m
  => HasUnit c u
  => ObjectOf c u
  => DictHasPure c m
  -> DictHasUnit c u
  -> Boolean
  -> m u
  -> m u
when _        _        true  m = m
when { pure } { unit } false _ = pure (unit Unit.unit)

instance hasPureUnrestricted
  :: Unrestricted.Applicative f
  => HasPure Function f
  where
  pure = Unrestricted.pure
