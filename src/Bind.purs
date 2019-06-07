module Control.Restricted.Bind
  ( class Bind
  ) where

import Control.Bind (class Bind) as Unrestricted
import Control.Restricted.Apply (class Apply)
import Control.Restricted.HasBind (class HasBind)

class (Apply c f, HasBind c f) <= Bind c f

instance bindUnrestricted
  :: Unrestricted.Bind m
  => Bind Function m
