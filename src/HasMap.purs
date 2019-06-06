module Control.Restricted.HasMap
  ( class HasMap
  , map        , (<$>)
  , mapFlipped , (<#>)
  ) where

import Record.Builder (Builder)
import Record.Builder (insert) as Builder
import Data.Symbol (SProxy(SProxy))

import Control.Restricted.Eval (class Eval, eval)
import Control.Restricted.ObjectOf (class ObjectOf, class TypeOperatorOf)
import Control.Restricted.Restrict (class Restrict, restrict)
import Data.Either (Either(Left, Right))
import Data.Functor (class Functor, map) as Unrestricted
import Data.Tuple (Tuple(Tuple))

class HasMap
  (c :: Type -> Type -> Type)
  (f :: Type -> Type)
  where
  map
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => c v0 v1
    -> f v0
    -> f v1

infixl 4 map as <$>

type DictHasMap c f =
  { map
      :: forall v0 v1
       . HasMap c f
      => ObjectOf c v0
      => ObjectOf c v1
      => c v0 v1
      -> f v0
      -> f v1
  }

mapFlipped
  :: forall c f v0 v1
   . HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => f v0
  -> c v0 v1
  -> f v1
mapFlipped fa f = f <$> fa

infixl 1 mapFlipped as <#>

flap
  :: forall c f v0 v1
   . Eval c
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v0 v1)
  => Restrict Function c
  => f (c v0 v1)
  -> v0
  -> f v1
flap ff x =
  dictHasMap.map (restrict (\f -> eval f x)) ff
  where
  dictHasMap :: DictHasMap c f
  dictHasMap = { map: map }

infixl 4 flap as <@>

type Ba0 = Builder {} { a0 :: Int }
insert_a0_0 :: Ba0
insert_a0_0 = Builder.insert (SProxy :: SProxy "a0") 0
type E_Ba0 = Either {} Ba0
e0 :: E_Ba0
e0 = Left {}
x1 :: Either {} { a0 :: Int }
x1 = e0 <@> {}
e1 :: E_Ba0
e1 = Right insert_a0_0
x2 :: Either {} { a0 :: Int }
x2 = e1 <@> {}

instance hasMapUnrestricted
  :: Unrestricted.Functor f
  => HasMap Function f
  where
  map = Unrestricted.map

instance hasMapBuilder
  :: ( Unrestricted.Functor f
     , TypeOperatorOf Builder f
     )
  => HasMap Builder f
  where
  map builder = Unrestricted.map (eval builder)

-- instance hasMapBuilderEither :: HasMap Builder (Either a) where
--   map builder = Unrestricted.map (eval builder)
-- instance hasMapBuilderTuple :: HasMap Builder (Tuple a) where
--   map builder = Unrestricted.map (eval builder)
