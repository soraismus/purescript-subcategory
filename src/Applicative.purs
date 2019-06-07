module Control.Restricted.Applicative
  ( class Applicative
  ) where

import Control.Applicative (class Applicative) as Unrestricted
import Control.Restricted.Apply (class Apply)
import Control.Restricted.HasPure (class HasPure)

class (Apply c f, HasPure c f) <= Applicative c f

instance applicativeUnrestricted
  :: Unrestricted.Applicative f
  => Applicative Function f
