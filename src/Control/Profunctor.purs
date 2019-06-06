module Control.Subcategories.Profunctor
  ( class Profunctor
  , dimap
  ) where

import Control.Subcategories.Category (class Category)
import Control.Subcategories.Identity (class Identity, identity)
import Control.Subcategories.ObjectOf (class ObjectOf)
import Control.Subcategories.Semigroupoid ((>>>))
import Data.Newtype (class Newtype, wrap, unwrap)
import Record.Builder (Builder)

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
   . Identity c
  => Identity p
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf p v0
  => Profunctor c p
  => c v0 v1
  -> p v0 v1
arr f = rmap f identity

lcmap
  :: forall c p v0 v1 v2
   . Identity c
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => Profunctor c p
  => c v0 v1
  -> p v1 v2
  -> p v0 v2
lcmap f = dimap f identity

rmap
  :: forall c p v0 v1 v2
   . Identity c
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => Profunctor c p
  => c v1 v2
  -> p v0 v1
  -> p v0 v2
rmap f = dimap identity f

instance profunctorFn :: Profunctor Function Function where
  dimap = dimap

instance profunctorBuilder :: Profunctor Builder Builder where
  dimap a2b c2d b2c = a2b >>> b2c >>> c2d
--   dimap a2b c2d b2c = mkBuilder a2b >>> b2c >>> mkBuilder c2d
--     where
--     mkBuilder
--       :: forall a b
--        . ObjectOf Builder a
--       => ObjectOf Builder b
--       => (a -> b)
--       -> Builder a b
--     mkBuilder = unsafeCoerce

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
