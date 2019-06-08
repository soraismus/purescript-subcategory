module Control.Subcategory.HasPure
  ( class HasPure
  , pure
  , pure'
  , unless
  , unless'
  , when
  , when'
  ) where

import Control.Applicative (class Applicative, pure) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasUnit (class HasUnit, unit')
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Data.Function (const) as Function
import Type.Proxy (Proxy3(Proxy3))

-- | The class `HasPure` registers for a type `c` and a type constructor `f`
-- | a distinguished function of type `forall a. a -> f a`.
class HasPure c f where
  pure :: forall v. ObjectOf c v => v -> f v
  pure' :: forall v. ObjectOf c v => Proxy3 c -> v -> f v

instance hasPureUnrestricted
  :: Unrestricted.Applicative f
  => HasPure Function f
  where
  pure = Unrestricted.pure
  pure' _ = Unrestricted.pure

else instance hasPure
  :: ( ObjectOf c v
     , Restrictable Function c
     )
  => HasPure c (c v)
  where
  pure x = restrict (Function.const x)
  pure' _ x = restrict (Function.const x)

inContext :: forall a b c. a -> (a -> b -> c) -> (a -> b) -> c
inContext context f0 f1 = f0 context (f1 context)

unless
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => ObjectOf c u
  => Boolean
  -> f u
  -> f u
unless false fu = fu
unless true  _  = inContext (Proxy3 :: Proxy3 c) pure' unit'

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
unless' c true  _  = inContext c pure' unit'

when
  :: forall c f u
   . HasPure c f
  => HasUnit c u
  => ObjectOf c u
  => Boolean
  -> f u
  -> f u
when true  fu = fu
when false _  = inContext (Proxy3 :: Proxy3 c) pure' unit'

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
when' c false _  = inContext c pure' unit'
