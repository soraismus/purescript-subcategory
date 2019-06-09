module Control.Subcategory.ClosedMonoidal
  ( class ClosedMonoidal
  ) where

import Control.Subcategory.ClosedSemimonoidal (class ClosedSemimonoidal)
import Control.Subcategory.HasExtrinsicUnit (class HasExtrinsicUnit)
import Data.Tuple (Tuple)
import Data.Unit (Unit)

class
  ( ClosedSemimonoidal c tensor
  , HasExtrinsicUnit c u0 u1
  )
  <= ClosedMonoidal c tensor u0 u1

instance closedMonoidalFunction :: ClosedMonoidal Function Tuple Unit Unit
