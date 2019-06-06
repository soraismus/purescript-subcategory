module Control.Restricted.Profunctor
  ( class Profunctor
  ) where

import Control.Restricted.Category (class Category)
import Control.Restricted.HasDimap (class HasDimap)

class (Category c, HasDimap c p) <= Profunctor c p
