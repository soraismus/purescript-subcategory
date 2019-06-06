module Control.Restricted.Profunctor
  ( class Profunctor
  ) where

import Control.Restricted.Category (class Category)
import Control.Restricted.HasDimap (class HasDimap)
import Record.Builder (Builder)

class (Category c, HasDimap c p) <= Profunctor c p

instance profunctorFn :: HasDimap Function p => Profunctor Function p

instance profunctorBuilder :: HasDimap Builder p => Profunctor Builder p
