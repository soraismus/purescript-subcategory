module Control.Subcategory.Monoidal
  ( class Monoidal
  ) where

import Control.Subcategory.HasExtrinsicUnit (class HasExtrinsicUnit)
import Control.Subcategory.Semimonoidal (class Semimonoidal)
import Data.Unit (Unit)

class
  ( Semimonoidal c bifunctor tensor
  , HasExtrinsicUnit c u0 u1
  )
  <= Monoidal c bifunctor tensor u0 u1

instance monoidalFunction :: Monoidal Function t t Unit Unit
