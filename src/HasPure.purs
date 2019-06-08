module Control.Restricted.HasPure
  ( class HasPure
  , liftA1
  , pure
  , unless
  , unless'
  , when
  , when'
  ) where

import Control.Applicative (class Applicative, pure) as Unrestricted
import Control.Restricted.HasApply (class HasApply, (<*>))
import Control.Restricted.HasUnit (class HasUnit, unit)
import Control.Restricted.ObjectOf (class ObjectOf)
import Type.Proxy (Proxy3(Proxy3))

class HasPure c f where
  pure :: forall v. ObjectOf c v => Proxy3 c -> v -> f v

-- | S combinator
inContext :: forall a b c. a -> (a -> b -> c) -> (a -> b) -> c
inContext context f0 f1 = f0 context (f1 context)

liftA1
  :: forall c f v0 v1
   . HasApply c f
  => HasPure c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v0 v1)
  => c v0 v1
  -> f v0
  -> f v1
liftA1 f x = pure (Proxy3 :: Proxy3 c) f <*> x

unless
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => ObjectOf c u
  => Boolean
  -> f u
  -> f u
unless false fu = fu
unless true  _  = inContext (Proxy3 :: Proxy3 c) pure unit

unless'
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => ObjectOf c u
  => Proxy3 c
  -> Boolean
  -> f u
  -> f u
unless' _ false fu = fu
unless' c true  _  = inContext c pure unit

when
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => ObjectOf c u
  => Boolean
  -> f u
  -> f u
when true  fu = fu
when false _  = inContext (Proxy3 :: Proxy3 c) pure unit

when'
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => ObjectOf c u
  => Proxy3 c
  -> Boolean
  -> f u
  -> f u
when' _ true  fu = fu
when' c false _  = inContext c pure unit

instance hasPureUnrestricted
  :: Unrestricted.Applicative f
  => HasPure Function f
  where
  pure _ = Unrestricted.pure
