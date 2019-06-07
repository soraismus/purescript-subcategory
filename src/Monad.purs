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
import Control.Restricted.HasPure (class HasPure, pure, unless, when)
import Control.Restricted.ObjectOf (class ObjectOf)
import Control.Restricted.Restrict (class Restrict, restrict)
import Data.Unit (Unit)

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

-- liftM1
--   :: forall c m v0 v1
--    . HasBind c m
--   => HasEval c
--   => HasPure c m
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => ObjectOf c (m v1)
--   => ObjectOf c (c v0 v1)
--   => Restrict Function c
--   => DictHasPure c m
--   -> c v0 v1
--   -> m v0
--   -> m v1
-- liftM1 { pure } f x =
--     dictHasBind.bind x $ restrict \x' -> pure $ eval f x'
--   where
--   dictHasBind :: DictHasBind c m
--   dictHasBind = { bind }

-- ap
--   :: forall c m v0 v1
--    . HasBind c m
--   => HasEval c
--   => HasPure c m
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => ObjectOf c (m v1)
--   => ObjectOf c (c v0 v1)
--   => Restrict Function c
--   => m (c v0 v1)
--   -> m v0
--   -> m v1
-- ap f x =
--   dictHasBind.bind f
--     $ restrict \f' -> dictHasBind.bind x
--     $ restrict \x' -> dictHasPure.pure $ eval f' x'
--   where
--   dictHasBind :: DictHasBind c m
--   dictHasBind = { bind }
--   dictHasPure :: DictHasPure c m
--   dictHasPure = { pure: pure }

-- whenM
--   :: forall c m
--    . HasBind c m
--   => Restrict Function c
--   => DictHasPure c m
--   -> m Boolean
--   -> m Unit
--   -> m Unit
-- whenM dictHasPure mb m =
--     mb >>= restrict \b -> when dictHasPure b m
--
-- unlessM
--   :: forall c m
--    . HasBind c m
--   => Restrict Function c
--   => DictHasPure c m
--   -> m Boolean
--   -> m Unit
--   -> m Unit
-- unlessM dictHasPure mb m =
--     mb >>= restrict \b -> unless dictHasPure b m
