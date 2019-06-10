module Control.Subcategory.Semimonoidal
  ( class Semimonoidal
  ) where

import Control.Subcategory.HasTJoin (class HasTJoin)

class HasTJoin c bf t <= Semimonoidal c bifunctor tensor

instance semimonoidal :: HasTJoin c bf t => Semimonoidal c bf t
