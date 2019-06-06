module Control.Subcategories.Eval
  (
    class Closed
  , class Eval
  , assertClosed
  , eval
  ) where

import Prelude (Unit, unit)

import Control.Subcategories.ObjectOf (class ObjectOf)
import Control.Subcategories.Category (class Category)
import Data.Function (apply) as Function
import Record.Builder (Builder)
import Record.Builder (build) as Builder
import Unsafe.Coerce (unsafeCoerce)

class Closed (c :: Type -> Type -> Type) where
  assertClosed :: forall a b. ObjectOf c (c a b) => Unit

instance closedFn :: Closed Function where
  assertClosed = unit

instance closedBuilder :: Closed Builder where
  assertClosed = unit

class TensorOf
  (c :: Type -> Type -> Type)
  (t :: Type -> Type -> Type)
  where
  assertTensor :: forall a b. ObjectOf c (t a b) => Unit

class UnitOf
  (c :: Type -> Type -> Type)
  (u :: Type)

class
  ( Category c
  , Closed c
--   , TensorOf c t
--   , UnitOf c u
  )
  <= Eval
      (c :: Type -> Type -> Type)
--       (t :: Type -> Type -> Type)
--       (u :: Type)
  where
--   curry
--     :: forall v0 v1 v2
--      . ObjectOf c v0
--     => ObjectOf c v1
--     => ObjectOf c v2
--     => c (t v0 v1) v2
--     -> c v0 (c v1 v2)
  eval
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v0 v1)
    -- => c (t (c v0 v1) v0) v1
    => (c v0 v1)
    -> v0
    -> v1

-- class (Category_ p, Profunctor_ p) <= Eval p where
--   eval :: forall a b. ObjectOf p a => ObjectOf p b => p a b -> a -> b

instance closedMonoidalFn :: Eval (->) where
  eval = Function.apply

instance closedMonoidalBuilder :: Eval Builder where
  eval builder record = coerceBuild Builder.build builder record
    where
    coerceBuild
      :: (forall r1 r2. Builder (Record r1) (Record r2) -> Record r1 -> Record r2)
      -> (forall a b . ObjectOf Builder a => ObjectOf Builder b => Builder a b -> a -> b)
    coerceBuild = unsafeCoerce


-- instance profunctor_Fn :: Profunctor_ (->) where
--   dimap_ = dimap
-- instance profunctorBuilder_ :: Profunctor_ Builder where
--   dimap_ a2b c2d b2c = mkBuilder a2b >>> b2c >>> mkBuilder c2d
--     where
--     mkBuilder
--       :: forall a b
--        . ObjectOf Builder a
--       => ObjectOf Builder b
--       => (a -> b)
--       -> Builder a b
--     mkBuilder = unsafeCoerce
