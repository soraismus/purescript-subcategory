module Control.Restricted.HasMap
  ( class HasMap
  , map        , (<$>)
  , mapFlipped , (<#>)
  ) where

import Control.Restricted.HasEval (class HasEval, eval)
import Control.Restricted.ObjectOf (class ObjectOf, class OperatorOf)
import Control.Restricted.Restrict (class Restrict, restrict)
import Data.Functor (class Functor, map) as Unrestricted
import Record.Builder (Builder)

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
   . HasEval c
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v0 v1)
  => Restrict Function c
  => f (c v0 v1)
  -> v0
  -> f v1
flap ff x = map consumeX ff
  where
  consumeX :: c (c v0 v1) v1
  consumeX = restrict (\f -> eval f x)

infixl 4 flap as <@>

instance hasMapUnrestricted
  :: Unrestricted.Functor f
  => HasMap Function f
  where
  map = Unrestricted.map

instance hasMapBuilder
  :: ( Unrestricted.Functor f
     , OperatorOf Builder f
     )
  => HasMap Builder f
  where
  map builder = Unrestricted.map (eval builder)
