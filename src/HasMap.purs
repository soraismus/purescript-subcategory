module Control.Restricted.HasMap
  ( class HasMap
  , map        , (<$>)
  , mapFlipped , (<#>)
  ) where

import Data.Unit (Unit)
import Data.Unit (unit) as Unit

import Control.Restricted.Eval (class Eval, eval)
import Control.Restricted.HasConst (class HasConst, const)
import Control.Restricted.HasDimap (class HasDimap, arr)
import Control.Restricted.HasIdentity (class HasIdentity, identity)
import Control.Restricted.HasUnit (class HasUnit, unit)
import Control.Restricted.ObjectOf (class ObjectOf)
import Control.Restricted.Restrict (class Restrict, restrict)
import Data.Functor (class Functor, map) as Unrestricted

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

-- type DictHasUnit c u = { unit :: HasUnit c u => ObjectOf c u => u }
-- type DictHasUnit c u = { unit :: ObjectOf c u => u }
-- type DictHasUnit c u = { unit :: HasUnit c u => u }
-- type DictHasUnit c u = { unit :: HasUnit c u => Unit -> u }
type DictHasUnit c u = { unit :: HasUnit c u => ObjectOf c u => Unit -> u }

void
  :: forall c f u v
   . HasConst c
  => HasMap c f
  => HasUnit c u
  => ObjectOf c v
  => DictHasUnit c u
  -> f v
  -> f u
void dictHasUnit = dictHasMap.map (const (dictHasUnit.unit Unit.unit))
  where
  dictHasMap :: DictHasMap c f
  dictHasMap = { map: map }

-- void
--   :: forall c f u v
--    . HasConst c
--   => HasMap c f
--   => HasUnit c u
--   => ObjectOf c v
--   => f v
--   -> f u
-- void = dictHasMap.map (const (dictHasUnit.unit Unit.unit))
--   where
--   dictHasMap :: DictHasMap c f
--   dictHasMap = { map: map }
--   dictHasUnit :: DictHasUnit c u
--   dictHasUnit = { unit: \_ -> unit }

-- voidLeft
--   :: forall c f v0 v1
--    . HasConst c
--   => HasMap c f
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => f v0
--   -> v1
--   -> f v1
-- voidLeft f x = const x <$> f
-- infixl 4 voidLeft as $>

-- voidRight
--   :: forall c f v0 v1
--    . HasConst c
--   => HasMap c f
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => v0
--   -> f v1
--   -> f v0
-- voidRight x = map (const x)
-- infixl 4 voidRight as <$

flap
  :: forall c f p v0 v1
   . HasDimap c p
  => Eval c
  => HasIdentity p
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

instance functorUnrestricted
  :: Unrestricted.Functor f
  => HasMap Function f
  where
  map = Unrestricted.map
