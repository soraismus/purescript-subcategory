module Control.Subcategory.HasExtrinsicUnit
  ( class HasExtrinsicUnit
  , extrinsicUnit
  , extrinsicUnit'
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasUnit (class HasUnit)
import Data.Function (identity) as Function
import Data.Unit (Unit)
import Type.Proxy (Proxy3(Proxy3))

class
  ( HasUnit c u1
  , ObjectOf c u1
  )
  <= HasExtrinsicUnit
      (c  :: Type -> Type -> Type)
      (u0 :: Type)
      (u1 :: Type)
      where
      extrinsicUnit' :: ObjectOf c u1 => Proxy3 c -> u0 -> u1

extrinsicUnit :: ObjectOf c u1 => u0 -> u1
extrinsicUnit = extrinsicUnit' (Proxy3 :: Proxy3 c)

instance hasExtrinsicUnitFunction :: HasExtrinsicUnit Function Unit Unit where
  extrinsicUnit = Function.identity
