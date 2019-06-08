module Control.Subcategory.Mapping.HasMap
  ( class HasMap_
  , map , (<$>)
  ) where

import Control.Subcategory.Constituency (class ObjectOf)

class HasMap_
  (c0 :: Type -> Type -> Type)
  (c1 :: Type -> Type -> Type)
  (f  :: Type -> Type)
  where
  map
    :: forall v0 v1
     . ObjectOf c0 v0
    => ObjectOf c0 v1
    => ObjectOf c1 (f v0)
    => ObjectOf c1 (f v1)
    => c0 v0 v1
    -> c1 (f v0) (f v1)

infixl 4 map as <$>

-- flap
--   :: forall c0 c1 f v0 v1
--    . HasMap_ c0 c1 f
--   => ObjectOf c0 v0
--   => ObjectOf c0 v1
--   => ObjectOf c0 (c0 v0 v1)
--   => ObjectOf c0 (c0 (c0 v0 v1) v1)
--   => ObjectOf c1 v0
--   => ObjectOf c1 (f v1)
--   => ObjectOf c1     (f (c0 v0 v1))
--   => ObjectOf c1 (c1 (f (c0 v0 v1)) (f v1))
--   => ObjectOf c1 (c1 (f (c0 (...) (...))) (f (f v1)))
--   => Restrictable Function c0
--   => Restrictable Function c1
--   => Slackable c0
--   => Slackable c1
--   => f (c0 v0 v1)
--   -> c1 v0 (f v1)
-- flap ff = restrict \v0 -> slackenC1 (map (slackenC0 consume v0)) ff
--   where
--   consume :: c0 v0 (c0 (c0 v0 v1) v1)
--   consume = restrict \v0 -> restrict (\f -> slackenC0 f v0)
--   slackenC0
--     :: forall v0 v1
--      . ObjectOf c0 v0
--     => ObjectOf c0 v1
--     => ObjectOf c0 (c0 v0 v1)
--     => (c0 v0 v1)
--     -> v0
--     -> v1
--   slackenC0 = slacken
--   slackenC1
--     :: forall v0 v1
--      . ObjectOf c1 v0
--     => ObjectOf c1 v1
--     => ObjectOf c1 (c1 v0 v1)
--     => ObjectOf c1 (c1 (f (c0 v0 v1)) (f v1))
--     => (c1 v0 v1)
--     -> v0
--     -> v1
--   slackenC1 = slacken
--
-- infixl 4 flap as <@>

-- instance hasMapUnrestricted
--   :: Unrestricted.Functor f
--   => HasMap Function f
--   where
--   map = Unrestricted.map
--
-- instance hasMapBuilder
--   :: ( Unrestricted.Functor f
--      , OperatorOf Builder f
--      )
--   => HasMap Builder f
--   where
--   map builder = Unrestricted.map (slacken builder)
