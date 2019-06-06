module Control.Profunctor_
  ( class Profunctor_
  , assertCategory_
  , dimap_
  ) where

import Prelude (unit)

import Control.Category_ (class Category_)
import Control.Identity_ (class Identity_, identity_)
import Control.ObjectOf (class ObjectOf)
import Data.Newtype (class Newtype, wrap, unwrap)
import Record.Builder (Builder)

class Profunctor_ c p where
  assertCategory_ :: Category_ c => Unit
  dimap_
    :: forall v0 v1 v2 v3
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => ObjectOf c v3
    => c v0 v1
    -> c v2 v3
    -> p v1 v2
    -> p v0 v3

arr_ :: forall c p v0 v1. Identity_ p => Profunctor_ c p => c v0 v1 -> p v0 v1
arr_ f = rmap_ f identity_

lcmap_
  :: forall c p v0 v1 v2
   . Identity_ c
  => Profunctor_ c p
  => c v0 v1
  -> p v1 v1
  -> p v0 v2
lcmap_ f = dimap_ f identity_

rmap_
  :: forall c p v0 v1 v2
   . Identity_ c
  => Profunctor_ c p
  => c v1 v2
  -> p v0 v1
  -> p v0 v2
rmap_ f = dimap_ identity_ f

instance profunctor_Fn :: Profunctor_ Function Function where
  assertCategory_ = unit
  dimap_ = dimap

instance profunctor_Builder :: Profunctor_ Builder Builder where
  assertCategory_ = unit
  dimap_ a2b c2d b2c = mkBuilder a2b >>> b2c >>> mkBuilder c2d
    where
    mkBuilder
      :: forall a b
       . ObjectOf Builder a
      => ObjectOf Builder b
      => (a -> b)
      -> Builder a b
    mkBuilder = unsafeCoerce

unwrapIso
  :: forall c p v0 v1
   . Newtype v0 v1
  => ObjectOf c v0
  => ObjectOf c v1
  => Profunctor_ c p
  -> p v0 v0
  -> p v1 v1
wrapIso = dimap_ unwrap wrap

wrapIso
  :: forall c p v0 v1
   . Newtype v0 v1
  => ObjectOf c v0
  => ObjectOf c v1
  => Profunctor_ c p
  => (v1 -> v0)
  -> p v1 v1
  -> p v0 v0
wrapIso _ = dimap_ wrap unwrap
