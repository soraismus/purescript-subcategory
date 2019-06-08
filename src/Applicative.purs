module Control.Subcategory.Applicative
  ( class Applicative
  ) where

import Control.Applicative (class Applicative) as Unrestricted
import Control.Subcategory.Apply (class Apply)
import Control.Subcategory.HasPure (class HasPure)

class (Apply c f, HasPure c f) <= Applicative c f

instance applicativeUnrestricted
  :: Unrestricted.Applicative f
  => Applicative Function f
