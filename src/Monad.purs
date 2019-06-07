module Control.Restricted.Monad
  ( class Monad
--   , ap
--   , liftM1
--   , whenM
--   , unlessM
  ) where

import Prelude (($))

import Control.Monad (class Monad) as Unrestricted
import Control.Restricted.Applicative (class Applicative)
import Control.Restricted.Apply (class Apply)
import Control.Restricted.Bind (class Bind)
import Control.Restricted.HasApply (class HasApply, apply)
import Control.Restricted.HasBind (class HasBind, bind, (>>=))
import Control.Restricted.HasEval (class HasEval, eval)
import Control.Restricted.HasPure (class HasPure, pure)
import Control.Restricted.ObjectOf (class ObjectOf)
import Control.Restricted.Restrict (class Restrict, restrict)

class (Applicative c m, Bind c m) <= Monad c m

type DictHasBind c m =
  { bind
      :: forall v0 v1
       . HasBind c m
      => ObjectOf c v0
      => ObjectOf c (m v1)
      => m v0
      -> c v0 (m v1)
      -> m v1
  }

type DictHasPure c f =
  { pure :: forall v. HasPure c f => ObjectOf c v => v -> f v }

instance monadUnrestricted
  :: Unrestricted.Monad m
  => Monad Function m

liftM1
  :: forall c m v0 v1
   . HasBind c m
  => HasEval c
  => HasPure c m
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v1)
  => Restrict Function c
  => c v0 v1
  -> m v0
  -> m v1
liftM1 f x =
    dictHasBind.bind x $ restrict \x' -> dictHasPure.pure (eval f x')
  where
  dictHasBind :: DictHasBind c m
  dictHasBind = { bind }
  dictHasPure :: DictHasPure c m
  dictHasPure = { pure }

-- ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
-- ap f a = do
--   f' <- f
--   a' <- a
--   pure (f' a')
--
-- whenM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
-- whenM mb m = do
--   b <- mb
--   when b m
--
-- unlessM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
-- unlessM mb m =  do
--   b <- mb
--   unless b m
