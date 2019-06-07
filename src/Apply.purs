module Control.Restricted.Apply
  ( class Apply
  ) where

import Control.Restricted.HasApply (class HasApply)
import Control.Restricted.Functor (class Functor)

class (Functor c f, HasApply c f) <= Apply c f
