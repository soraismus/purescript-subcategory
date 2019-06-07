module Control.Restricted.HasApply
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
import Control.Restricted.HasConst (class HasConst, const)
import Control.Restricted.HasEval (class HasEval, eval)
import Control.Restricted.HasIdentity (class HasIdentity, identity)
import Control.Restricted.HasMap (class HasMap, map, (<$>))
import Control.Restricted.ObjectOf (class ObjectOf)
--import Data.Functor (class Functor, map) as Unrestricted
--import Record.Builder (Builder)

-- class Strength f t where
--   strengthen :: forall v0 v1. t v0 (f v1) -> f (t v0 v1)
-- class Semimonoidal f t
--   join :: forall v0 v1 => t (f v0) (f v1) -> f (t v0 v1)
-- class Semimonoidal_ f t0 t1
--   join_ :: forall v0 v1 => t1 (f v0) (f v1) -> f (t0 v0 v1)

-- Functor f, HasApply <= ... Apply

-- | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
-- Tuple
--   (f (c v0 v1))
--   (f v0)
-- --> f (Tuple (c v0 v1) v0)
-- --> f v1
class HasApply c f where
  apply
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v0 v1)
    => f (c v0 v1)
    -> f v0
    -> f v1

infixl 4 apply as <*>

type DictHasApply c f =
  { apply
      :: forall v0 v1
       . HasApply c f
      => ObjectOf c v0
      => ObjectOf c v1
      => ObjectOf c (c v0 v1)
      => f (c v0 v1)
      -> f v0
      -> f v1
  }

instance applyUnrestricted :: Unrestricted.Apply f => HasApply Function f where
  apply = Unrestricted.apply

-- instance applyBuilder
--   :: ObjectOf Builder r
--   => HasApply Builder (Builder r)
--   where
--   apply ff fx r = eval (eval ff r) (eval fx r)

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
applyFirst x0 x1 = dictHasApply.apply (const <$> x0) x1
  where
  dictHasApply :: DictHasApply c f
  dictHasApply = { apply: apply }

infixl 4 applyFirst as <*

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

applySecond
  :: forall c f v0 v1
   . HasApply c f
  => HasConst c
  => HasEval c
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
    dictHasApply.apply
      (dictHasMap.map (eval const identity) x0)
      x1
  where
  dictHasMap :: DictHasMap c f
  dictHasMap = { map: map }
  dictHasApply :: DictHasApply c f
  dictHasApply = { apply: apply }

infixl 4 applySecond as *>

-- map   :: c v0 v1 -> f v0 -> f v1
-- apply :: f (c v0 v1) -> f v0 -> f v1
-- eval  :: (c v0 v1) -> v0 -> v1
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
lift2 f x0 x1 = apply (map f x0) x1

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
