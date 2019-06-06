module Control.Monoidal_
  where

class Monoidal_ c t u where
  assertSemimonoidal_ :: Semimonoidal_ c t => Unit
  assertUnit :: UnitOf c u => Unit

-- class Monoidal_ c t where
--   assertTensor :: TensorOf c t => Unit
--   unit_ :: f Unit ->
--   join_
--     :: forall a b t
--      . ObjectOf c a
--     => ObjectOf c b
--     => ObjectOf c (t a b)
--     => t (f a) (f b)
--     -> f (t a b)

--
-- instance x
--   :: TensorOf Function t
--   => Monoidal_ Function t
