module Control.Subcategory.ClosedMonoidal
  ( class ClosedMonoidal
  ) where

import Control.Subcategory.ClosedSemimonoidal (class ClosedSemimonoidal)
import Control.Subcategory.HasExtrinsicUnit (class HasExtrinsicUnit)

class
  ( ClosedSemimonoidal c tensor
  , HasExtrinsicUnit c u0 u1
  )
  <= ClosedMonoidal c tensor u0 u1

instance closedMonoidal
  :: ( ClosedSemimonoidal c tensor
     , HasExtrinsicUnit c u0 u1
     )
  => ClosedMonoidal c tensor u0 u1
