module Control.Functor__
  ( class Functor__
  , assertSrcCategory_
  , assertTgtCategory_
  , map__, (<$>)
  ) where

import Prelude (unit)

import Control.ObjectOf (class ObjectOf)
import Control.Category_ (class Category_)
import Data.Functor (map)

class Functor__ c0 c1 f where
  assertSrcCategory_ :: Category_ c0 => Unit
  assertTgtCategory_ :: Category_ c1 => Unit
  map__
    :: forall a b
     . ObjectOf c0 a
    => ObjectOf c0 b
    => ObjectOf c1 (f a)
    => ObjectOf c1 (f b)
    -> c0 a b
    -> c1 (f a) (f b)

infixl 4 map__ as <$>

instance functor__Functor :: Functor f => Functor_ Function Function f where
  assertSrcCategory_ = unit
  assertTgtCategory_ = unit
  map__ = map
