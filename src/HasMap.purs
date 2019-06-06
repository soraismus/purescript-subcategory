module Control.Restricted.HasMap
  ( class HasMap
  , map        , (<$>)
  , mapFlipped , (<#>)
  ) where

import Data.Identity (Identity(Identity))
import Type.Proxy (Proxy3(Proxy3))
import Record.Builder (Builder)
import Record.Builder (build, insert) as Builder
import Data.Either (Either(Left, Right))
import Data.Symbol (SProxy(SProxy))

import Data.Unit (Unit)
import Data.Unit (unit) as Unit

import Control.Restricted.Eval (class Eval, eval)
import Control.Restricted.HasConst (class HasConst, const)
import Control.Restricted.HasDimap (class HasDimap)
-- import Control.Restricted.HasIdentity (class HasIdentity, identity)
import Control.Restricted.HasIdentity (class HasIdentity)
import Control.Restricted.HasUnit (class HasUnit, unit)
import Control.Restricted.HasUnit (class HasUnit)
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

type Ba0 = Builder {} { a0 :: Int }
insert_a0_0 :: Ba0
insert_a0_0 = Builder.insert (SProxy :: SProxy "a0") 0

type E_Ba0 = Either {} Ba0
-- x :: E_Ba0
-- x = flap (Left {} :: E_Ba0)

instance hasMapUnrestricted
  :: Unrestricted.Functor f
  => HasMap Function f
  where
  map = Unrestricted.map

instance hasMapBuilderEither
  :: HasMap Builder (Either r)
  where
  map builder (Left r) = Left r
  map builder (Right x) = Right (eval builder x)
