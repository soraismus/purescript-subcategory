module Control.Subcategory.Applicative
  ( class Applicative
  , liftA1
  ) where

import Control.Applicative (class Applicative) as Unrestricted
import Control.Subcategory.Apply (class Apply)
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasApply (class HasApply, (<*>))
import Control.Subcategory.HasPure (class HasPure, pure)
import Type.Proxy (Proxy3(Proxy3))

class (Apply c f, HasPure c f) <= Applicative c f

instance applicativeUnrestricted
  :: Unrestricted.Applicative f
  => Applicative Function f

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
