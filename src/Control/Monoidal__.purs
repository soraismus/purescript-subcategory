module Control.Monoidal__
  where
--
-- class Monoidal__ c0 c1 f where
--   assertSrcCategory_ :: Category_ c0 m0 => Unit
--   assertTgtCategory_ :: Category_ c1 m1 => Unit
--   assertFunctor__ :: Functor__ c0 c1 f => Unit
--   unit_ :: forall u0 u1. UnitOf c0 u0 => UnitOf c1 u1 => u1 -> f u0
--   join_
--     :: forall m0 m1 a b
--      . ObjectOf c0 a
--     => Object c0 b
--     => ObjectOf c1 (f a)
--     => ObjectOf c1 (f b)
--     => TensorOf c0 t0
--     => TensorOf c1 t1
--     => t1 (f a) (f b)
--     -> f (t0 a b)
