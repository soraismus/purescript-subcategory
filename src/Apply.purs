module Control.Subcategory.Apply
  ( class Apply
  ) where

import Control.Apply (class Apply) as Unrestricted
import Control.Subcategory.HasApply (class HasApply)
import Control.Subcategory.Functor (class Functor)

-- | `Apply` represents a strong lax semi-monoidal endofunctor.
-- |
-- | Instances must satisfy the following law in addition
-- | to the `Functor` laws:
-- |
-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
class (Functor c f, HasApply c f) <= Apply c f

instance applyUnrestricted :: Unrestricted.Apply f => Apply Function f
