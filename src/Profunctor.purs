module Control.Restricted.Profunctor
  ( class Profunctor
  , arr
  , dimap
  , lcmap
  , rmap
  , unwrapIso
  , wrapIso
  ) where

import Control.Restricted.Category (class Category)
import Control.Restricted.Identity (class HasIdentity, identity)
import Control.Restricted.ObjectOf (class ObjectOf)
import Control.Restricted.Semigroupoid ((>>>))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Profunctor (class Profunctor, dimap) as Unrestricted

class Category c <= Profunctor
  (c :: Type -> Type -> Type)
  (p :: Type -> Type -> Type)
  where
  dimap
    :: forall v0 v1 v2 v3
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => ObjectOf c v3
    => c v0 v1
    -> c v2 v3
    -> p v1 v2
    -> p v0 v3

arr
  :: forall c p v0 v1
   . HasIdentity p
  => ObjectOf c v0
  => ObjectOf c v1
  => Profunctor c p
  => c v0 v1
  -> p v0 v1
arr f = rmap f identity

lcmap
  :: forall c p v0 v1 v2
   . ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => Profunctor c p
  => c v0 v1
  -> p v1 v2
  -> p v0 v2
lcmap f = dimap f identity

rmap
  :: forall c p v0 v1 v2
   . ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => Profunctor c p
  => c v1 v2
  -> p v0 v1
  -> p v0 v2
rmap f = dimap identity f

unwrapIso
  :: forall p v0 v1
   . Newtype v0 v1
  => ObjectOf Function v1
  => ObjectOf Function v0
  => Profunctor Function p
  => p v1 v1
  -> p v0 v0
unwrapIso = dimap unwrap wrap

wrapIso
  :: forall p v0 v1
   . Newtype v0 v1
  => ObjectOf Function v1
  => ObjectOf Function v0
  => Profunctor Function p
  => (v0 -> v1)
  -> p v0 v0
  -> p v1 v1
wrapIso _ = dimap wrap unwrap

instance profunctorUnrestricted :: Unrestricted.Profunctor p => Profunctor Function p where
  dimap = Unrestricted.dimap
else instance profunctorCategory :: Category c => Profunctor c c where
  dimap a2b c2d b2c = a2b >>> b2c >>> c2d
