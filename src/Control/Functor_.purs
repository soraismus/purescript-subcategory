module Control.Functor_
  ( class Functor_
  , assertCategory_
  , map_, (<$>)
  ) where

import Prelude (unit)

import Control.ObjectOf (class ObjectOf)
import Control.Category_ (class Category_)
import Control.Semigroupoid_ (compose_)
import Control.UnitOf (class UnitOf)
import Data.Functor (map)

class Functor_ c f where
  assertCategory_ :: Category_ c => Unit
  map_ :: forall a b. ObjectOf c a => ObjectOf c b => c a b -> f a -> f b

infixl 4 map_ as <$>

instance functor_Functor :: Functor f => Functor_ Function f where
  assertCategory_ = unit
  map_ = map

instance functor_Builder_Builder
  :: ObjectOf Builder a
  => Functor_ Builder (Builder a)
  where
  assertCategory_ = unit
  map_ = compose_

-- flap :: forall f a b. Functor f => f (a -> b) -> a -> f b
-- flap ff x = map (\f -> f x) ff
-- flap_
--   :: forall a b f p
--    . Functor_ p f
--   => ObjectOf p (p a b)
--   => ObjectOf p b
--   -> f (p a b)     --  Builder e (Builder a b) ; eval_ :: Builder (Builder a b) b
--   -> a
--   -> f b
-- flap_ x y = map_ (\f -> f y) x
--   map_ (createPArrow (\pArrow -> eval_ pArrow y)) x
-- eval_ :: p a b -> a -> b
-- map_ (arr_ (\builder -> eval_ builder y)) x
-- arr_
--   :: forall a b p
--    . Category_ p
--   => ObjectOf p a
--   => ObjectOf p b
--   => Profunctor_ p
--   => (a -> b)
--   -> p a b
-- infixl 4 flap as <@>
