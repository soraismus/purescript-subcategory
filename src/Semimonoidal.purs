module Control.Subcategory.Semimonoidal
  ( class Semimonoidal
  , join
  ) where

import Prelude ((<<<))

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasUnit (class HasUnit)
import Data.Function (flip, identity) as Function
import Data.Tuple (Tuple)
import Data.Tuple (curry, swap, uncurry) as Tuple
import Data.Unit (Unit)

class Semimonoidal
  (c         :: Type -> Type -> Type)
  (bifunctor :: Type -> Type -> Type)
  (tensor    :: Type -> Type -> Type)
  where
  join
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (tensor v0 v1)
    => bifunctor v0 v1
    -> tensor v0 v1

instance semimonoidalFunction :: Semimonoidal Function t t where
  join = Function.identity
