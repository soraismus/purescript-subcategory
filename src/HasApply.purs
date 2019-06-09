module Control.Subcategory.HasApply
  ( class HasApply
  , apply       , (<*>)
  , applyFirst  , (<*)
  , applySecond , (*>)
  , lift2
  , lift3
  , lift4
  , lift5
  ) where

import Control.Apply (class Apply, apply) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasConst (class HasConst, const)
import Control.Subcategory.HasIdentity (class HasIdentity, identity)
import Control.Subcategory.Functor.HasMap (class HasMap, map, (<$>))
import Control.Subcategory.Slackable (class Slackable, slacken)

-- class Strength f t where
--   strengthen :: forall v0 v1. t v0 (f v1) -> f (t v0 v1)
-- class Semimonoidal f t
--   join :: forall v0 v1 => t (f v0) (f v1) -> f (t v0 v1)
-- class Semimonoidal_ f t0 t1
--   join_ :: forall v0 v1 => t1 (f v0) (f v1) -> f (t0 v0 v1)

-- | The `HasApply` class provides the `(<*>)` which is used to apply a
-- | function to an argument under a type constructor.
-- |
-- | `HasApply` can be used to lift functions of two or more arguments to work
-- | on values wrapped with the type constructor `f`. It might also be
-- | understood in terms of the `lift2` function:
-- |
-- | ```purescript
-- | lift2 :: forall f a b c. HasApply f => (a -> b -> c) -> f a -> f b -> f c
-- | lift2 f a b = f <$> a <*> b
-- | ```
-- |
-- | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
-- | the function application operator `($)` to arguments wrapped with the
-- | type constructor `f`.
class HasApply c f where
  apply
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v0 v1)
    => f (c v0 v1)
    -> f v0
    -> f v1

--
-- if c := (->)
--
--    f (c0 v0 v1)            -> c1 (f v0) (f v1)
--    |
--    | uncurry c1
--    v
-- t1 (f (c0 v0 v1)) (f v0)   ->           f v1
--    |
--    | semimonoidal f
--    v
-- f (t1 (c0 v0 v1) v0)       ->           f v1
--    |
--    | map eval 1/0?     [map (slacken eval) -- slacken isn't necessary]
--    v
-- f (v1)                    ->           f v1
--
--
-- HasApply maps from category c to category (->)
--   so currying and uncurrying are possible
--
--         eval :: forall v0 v1.   c (t (c v0 v1) v0)      v1
-- slacken eval :: forall v0 v1.      t (c v0 v1) v0  ->   v1
-- map     eval :: forall f v0 v1. f (t (c v0 v1) v0) -> f v1


infixl 4 apply as <*>

-- apply'
--   :: forall c f v0 v1
--    . HasApply c f
--   => Slackable c
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => ObjectOf c (c v0 v1)
--   => Restrictable Function c
--   => f (c v0 v1)
--   -> f v0
--   -> f v1
-- -- apply' = lift2 (restrict slacken)
-- -- apply' = lift2 (restrict \v0 -> slacken slacken v0)
-- apply' ff fx0
--
-- slacken' :: c v0 v1 -> v1
-- slacken' f = slacken f x0
-- slacken'' :: c (c v0 v1) v1
-- slacken'' = restrict slacken'
--
-- ($) :: forall a b. (a -> b) -> a -> b
-- eval
--   => (c v0 v1)
--   -> v0
--   -> v1
-- lift2
--   => c v0 (c v1 v2)
--   -> f v0
--   -> f v1
--   -> f v2

applyFirst
  :: forall c f v0 v1
   . HasApply c f
  => HasConst c
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v1 v0)
  => f v0
  -> f v1
  -> f v0
applyFirst x0 x1 = apply constX0 x1
  where
  constX0 :: f (c v1 v0)
  constX0 = const <$> x0

infixl 4 applyFirst as <*

applySecond
  :: forall c f v0 v1
   . HasApply c f
  => HasConst c
  => Slackable c
  => HasIdentity c
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v1 v1)
  => ObjectOf c (c v0 (c v1 v1))
  => ObjectOf c (c (c v1 v1) (c v0 (c v1 v1)))
  => f v0
  -> f v1
  -> f v1
applySecond x0 x1 =
    apply (map evalConstIdentity x0) x1
  where
  evalConstIdentity :: c v0 (c v1 v1)
  evalConstIdentity = slacken const identity

infixl 4 applySecond as *>

lift2
  :: forall c f v0 v1 v2
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c (c v1 v2)
  => c v0 (c v1 v2)
  -> f v0
  -> f v1
  -> f v2
lift2 f x0 x1 = f <$> x0 <*> x1

lift3
  :: forall c f v0 v1 v2 v3
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c v3
  => ObjectOf c (c v2 v3)
  => ObjectOf c (c v1 (c v2 v3))
  => c v0 (c v1 (c v2 v3))
  -> f v0
  -> f v1
  -> f v2
  -> f v3
lift3 f x0 x1 x2 = f <$> x0 <*> x1 <*> x2

lift4
  :: forall c f v0 v1 v2 v3 v4
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c v3
  => ObjectOf c v4
  => ObjectOf c (c v3 v4)
  => ObjectOf c (c v2 (c v3 v4))
  => ObjectOf c (c v1 (c v2 (c v3 v4)))
  => c v0 (c v1 (c v2 (c v3 v4)))
  -> f v0
  -> f v1
  -> f v2
  -> f v3
  -> f v4
lift4 f x0 x1 x2 x3 = f <$> x0 <*> x1 <*> x2 <*> x3

lift5
  :: forall c f v0 v1 v2 v3 v4 v5
   . HasApply c f
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c v2
  => ObjectOf c v3
  => ObjectOf c v4
  => ObjectOf c v5
  => ObjectOf c (c v4 v5)
  => ObjectOf c (c v3 (c v4 v5))
  => ObjectOf c (c v2 (c v3 (c v4 v5)))
  => ObjectOf c (c v1 (c v2 (c v3 (c v4 v5))))
  => c v0 (c v1 (c v2 (c v3 (c v4 v5))))
  -> f v0
  -> f v1
  -> f v2
  -> f v3
  -> f v4
  -> f v5
lift5 f x0 x1 x2 x3 x4 = f <$> x0 <*> x1 <*> x2 <*> x3 <*> x4

instance applyUnrestricted :: Unrestricted.Apply f => HasApply Function f where
  apply = Unrestricted.apply





-- mapFlipped
--   :: forall c f v0 v1
--    . HasMap c f
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => f v0
--   -> c v0 v1
--   -> f v1
-- mapFlipped fa f = f <$> fa
--
-- infixl 1 mapFlipped as <#>
