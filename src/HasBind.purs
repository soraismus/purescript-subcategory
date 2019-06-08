module Control.Restricted.HasBind
  ( class HasBind
  , class Discard
  , class Discard_
  , bind                  , (>>=)
  , bindFlipped           , (=<<)
  , discard
  , discard'
  , join
  , composeKleisli        , (>=>)
  , composeKleisliFlipped , (<=<)
  , ifM
  ) where

import Prelude (($))

import Control.Bind (class Bind, class Discard, bind) as Unrestricted
import Control.Restricted.HasEval (class HasEval, eval)
import Control.Restricted.HasIdentity (class HasIdentity, identity)
import Control.Restricted.HasUnit (class HasUnit)
import Control.Restricted.ObjectOf (class ObjectOf)
import Control.Restricted.Restrict (class Restrict, restrict)
import Data.Function (flip)

class HasBind c m where
  bind
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c (m v1)
    => m v0
    -> c v0 (m v1)
    -> m v1

infixl 1 bind as >>=

bindFlipped
  :: forall c m v0 v1
   . HasBind c m
  => ObjectOf c v0
  => ObjectOf c (m v1)
  => c v0 (m v1)
  -> m v0
  -> m v1
bindFlipped = flip bind

infixr 1 bindFlipped as =<<

instance bindUnrestricted
  :: Unrestricted.Bind m
  => HasBind Function m
  where
  bind = Unrestricted.bind

class Discard_ c f a where
  discard'
    :: forall v
     . HasBind c f
    => ObjectOf c a
    => ObjectOf c (f v)
    => f a
    -> c a (f v)
    -> f v

instance discard_Unrestricted
  :: ( HasBind Function f
     , Unrestricted.Bind f
     , Unrestricted.Discard a
     )
  => Discard_ Function f a
  where
  discard' = Unrestricted.bind

class Discard c a where
  discard
    :: forall f v
     . HasBind c f
    => ObjectOf c a
    => ObjectOf c (f v)
    => f a
    -> c a (f v)
    -> f v

instance discardUnit :: HasUnit c u => Discard c u where
  discard = bind

join
  :: forall c m v
   . HasBind c m
  => HasIdentity c
  => ObjectOf c (m v)
  => m (m v)
  -> m v
join m = bindM identity
  where
  bindM :: c (m v) (m v) -> m v
  bindM = bind m

composeKleisli
  :: forall c m v0 v1 v2
   . HasBind c m
  => HasEval c
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v1)
  => ObjectOf c (m v2)
  => ObjectOf c (c v0 (m v1))
  => c v0 (m v1)
  -> c v1 (m v2)
  -> v0
  -> m v2
composeKleisli f g a = eval f a >>= g

infixr 1 composeKleisli as >=>

composeKleisliFlipped
  :: forall c m v0 v1 v2
   . HasBind c m
  => HasEval c
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v1)
  => ObjectOf c (m v2)
  => ObjectOf c (c v0 (m v1))
  => c v1 (m v2)
  -> c v0 (m v1)
  -> v0
  -> m v2
composeKleisliFlipped f g a = f =<< eval g a

infixr 1 composeKleisliFlipped as <=<

ifM
  :: forall c m v
   . HasBind c m
  => ObjectOf c Boolean
  => ObjectOf c (m v)
  => Restrict Function c
  => m Boolean
  -> m v
  -> m v
  -> m v
ifM mCond mt mf =
    bindCond $ restrict (if _ then mt else mf)
  where
  bindCond :: c Boolean (m v) -> m v
  bindCond = bind mCond
