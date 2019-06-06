module Control.Restricted.Functor
  ( class Functor
  ) where

import Control.Restricted.Category (class Category)
import Control.Restricted.HasMap (class HasMap)

class (Category c, HasMap c f) <= Functor c f
