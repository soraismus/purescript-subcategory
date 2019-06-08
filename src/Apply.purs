module Control.Subcategory.Apply
  ( class Apply
  ) where

import Control.Apply (class Apply) as Unrestricted
import Control.Subcategory.HasApply (class HasApply)
import Control.Subcategory.Functor (class Functor)

class (Functor c f, HasApply c f) <= Apply c f

instance applyUnrestricted
  :: Unrestricted.Apply f
  => Apply Function f
