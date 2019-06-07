module Control.Restricted.HasApply
  ( class HasApply, apply, (<*>)
  , applyFirst, (<*)
  , applySecond, (*>)
--   , lift2, lift3, lift4, lift5
  ) where

import Control.Apply (class Apply, apply) as Unrestricted
-- import Control.Category (identity)
import Control.Restricted.HasConst (class HasConst, const)
import Control.Restricted.HasEval (class HasEval, eval)
import Control.Restricted.HasIdentity (class HasIdentity, identity)
import Control.Restricted.HasMap
  ( class HasMap
  , map
  , (<#>)
  , (<$>)
  )
import Control.Restricted.ObjectOf (class ObjectOf)
import Data.Functor (class Functor, map) as Unrestricted
import Data.Function (const) as Function
import Record.Builder (Builder)

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

instance applyUnrestricted :: Unrestricted.Apply f => HasApply Function f where
  apply = Unrestricted.apply

-- instance applyBuilder
--   :: ObjectOf Builder r
--   => HasApply Builder (Builder r)
--   where
--   apply ff fx r = eval (eval ff r) (eval fx r)

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
applySecond x0 x1 = dictHasApply.apply (dictHasMap.map (eval const identity) x0) x1
  where
  dictHasMap :: DictHasMap c f
  dictHasMap = { map: map }
  dictHasApply :: DictHasApply c f
  dictHasApply = { apply: apply }

infixl 4 applySecond as *>

-- lift2
--   :: forall c f v0 v1 v2
--    . Apply c f
--   => (v0 -> v1 -> v2)
--   -> c ((f v0) (c (f v1) (f v2)))
-- lift2 f a v1 = f <$> a <*> b

-- lift3 :: forall v0 v1 v2 v3 f. Apply f => (v0 -> v1 -> v2 -> v3) -> f v0 -> f v1 -> f v2 -> f d
-- lift3 f a v1 v2 = f <$> a <*> v1 <*> c
-- lift4 :: forall v0 v1 v2 v3 v4 f. Apply f => (v0 -> v1 -> v2 -> v3 -> v4) -> f v0 -> f v1 -> f v2 -> f v3 -> f e
-- lift4 f a v1 v2 v3 = f <$> a <*> v1 <*> v2 <*> d
-- lift5 :: forall v0 v1 v2 v3 v4 f v6. Apply f => (v0 -> v1 -> v2 -> v3 -> v4 -> v6) -> f v0 -> f v1 -> f v2 -> f v3 -> f v4 -> f g
-- lift5 f a v1 v2 v3 v4 = f <$> a <*> v1 <*> v2 <*> v3 <*> e
