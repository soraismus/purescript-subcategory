module Control.Subcategory.Bind
  ( class Bind
  ) where

import Control.Bind (class Bind) as Unrestricted
import Control.Subcategory.Apply (class Apply)
import Control.Subcategory.HasBind (class HasBind)

class (Apply c f, HasBind c f) <= Bind c f

instance bindUnrestricted :: Unrestricted.Bind m => Bind Function m
