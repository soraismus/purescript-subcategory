module Control.ClosedMonoidal_
  ( class ClosedMonoidal_
      , assertCategory_
      , eval_
  ) where

import Control.ObjectOf (class ObjectOf)
import Control.Semigroupoid_ (class Semigroupoid_)
import Control.Category_ (class Category_)

class ClosedMonoidal_ c t u where
  assertClosed :: forall a b. ObjectOf c (c a b) => Unit
  assertTensor :: forall a b. ObjectOf c (t a b) => Unit
  assertUnit :: ObjectOf c u => Unit
  curry
    :: forall v0 v1 v2
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => c (t v0 v1) v2
    -> c v0 (c v1 v2)
  eval
    :: forall a b
     . ObjectOf c a
    => ObjectOf c b
    -- 0. -- => c (t (c v0 v1) v0) v1
    -- 1. -- => (c v0 v1) -> v0 -> v1


class (Category_ p, Profunctor_ p) <= ClosedMonoidal_ p where
  eval_ :: forall a b. ObjectOf p a => ObjectOf p b => p a b -> a -> b

instance closedMonoidalFn_ :: ClosedMonoidal_ (->) where
  eval_ = Function.apply
instance closedMonoidalBuilder_ :: ClosedMonoidal_ Builder where
  eval_ builder record = coerceBuild Builder.build builder record
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
