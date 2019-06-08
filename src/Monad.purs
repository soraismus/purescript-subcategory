module Control.Subcategory.Monad
  ( class Monad
  , ap
  , liftM1
  , unlessM
  , unlessM'
  , whenM
  , whenM'
  ) where

import Prelude (($))

import Control.Monad (class Monad) as Unrestricted
import Control.Subcategory.Applicative (class Applicative)
import Control.Subcategory.Bind (class Bind)
import Control.Subcategory.HasBind (class HasBind, bind)
import Control.Subcategory.HasEval (class HasEval, eval)
import Control.Subcategory.HasPure (class HasPure, pure, unless', when')
import Control.Subcategory.HasUnit (class HasUnit)
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Type.Proxy (Proxy3(Proxy3))

class (Applicative c m, Bind c m) <= Monad c m

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
  => ObjectOf c (c v0 v1)
  => Restrictable Function c
  => c v0 v1
  -> m v0
  -> m v1
liftM1 f mx =
    bindMx $ restrict \x -> pure c $ eval f x
  where
  bindMx :: c v0 (m v1) -> m v1
  bindMx = bind mx
  c = Proxy3 :: Proxy3 c

ap
  :: forall c m v0 v1
   . HasBind c m
  => HasEval c
  => HasPure c m
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v1)
  => ObjectOf c (c v0 v1)
  => Restrictable Function c
  => m (c v0 v1)
  -> m v0
  -> m v1
ap mf mx =
  bindF
    $ restrict \f -> bindX
    $ restrict \x -> pure c $ eval f x
  where
  bindF :: c (c v0 v1) (m v1) -> m v1
  bindF = bind mf
  bindX :: c v0 (m v1) -> m v1
  bindX = bind mx
  c = Proxy3 :: Proxy3 c

unlessM
  :: forall c m u
   . HasBind c m
  => HasPure c m
  => HasUnit c u
  => ObjectOf c Boolean
  => ObjectOf c u
  => ObjectOf c (m u)
  => Restrictable Function c
  => m Boolean
  -> m u
  -> m u
unlessM mCond mUnit =
    bindCond $ restrict \cond -> unless' c cond mUnit
  where
  bindCond :: c Boolean (m u) -> m u
  bindCond = bind mCond
  c = Proxy3 :: Proxy3 c

unlessM'
  :: forall c m u
   . HasBind c m
  => HasPure c m
  => HasUnit c u
  => ObjectOf c Boolean
  => ObjectOf c u
  => ObjectOf c (m u)
  => Restrictable Function c
  => Proxy3 c
  -> m Boolean
  -> m u
  -> m u
unlessM' c mCond mUnit =
    bindCond $ restrict \cond -> unless' c cond mUnit
  where
  bindCond :: c Boolean (m u) -> m u
  bindCond = bind mCond

whenM
  :: forall c m u
   . HasBind c m
  => HasPure c m
  => HasUnit c u
  => ObjectOf c Boolean
  => ObjectOf c u
  => ObjectOf c (m u)
  => Restrictable Function c
  => m Boolean
  -> m u
  -> m u
whenM mCond mUnit =
    bindCond $ restrict \b -> when' c b mUnit
  where
  bindCond :: c Boolean (m u) -> m u
  bindCond = bind mCond
  c = Proxy3 :: Proxy3 c

whenM'
  :: forall c m u
   . HasBind c m
  => HasPure c m
  => HasUnit c u
  => ObjectOf c Boolean
  => ObjectOf c u
  => ObjectOf c (m u)
  => Restrictable Function c
  => Proxy3 c
  -> m Boolean
  -> m u
  -> m u
whenM' c mCond mUnit =
    bindCond $ restrict \b -> when' c b mUnit
  where
  bindCond :: c Boolean (m u) -> m u
  bindCond = bind mCond
