module Control.Restricted.HasBind
  ( class HasBind
  , bind                  , (>>=)
  , bindFlipped           , (=<<)
--   , class Discard, discard
  , join
  , composeKleisli        , (>=>)
  , composeKleisliFlipped , (<=<)
  , ifM
  ) where

import Prelude (($))

import Control.Bind (class Bind, bind) as Unrestricted
import Control.Restricted.HasEval (class HasEval, eval)
import Control.Restricted.HasIdentity (class HasIdentity, identity)
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

-- class Discard c a where
--   discard
--     :: forall f v
--      . HasBind c f
--     => ObjectOf c a
--     => ObjectOf c (f v)
--     => f a
--     -> c a (f v)
--     -> f v

-- instance discardUnrestricted
--   :: Unrestricted.Discard Unit
--   => Discard Function Unit
--   where
--   discard = Unrestricted.bind

join
  :: forall c m v
   . HasBind c m
  => HasIdentity c
  => ObjectOf c (m v)
  => m (m v)
  -> m v
join m = dictHasBind.bind m identity
  where
  dictHasBind :: DictHasBind c m
  dictHasBind = { bind }

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
ifM cond t f =
    dictHasBind.bind cond $ restrict \cond' -> if cond' then t else f
  where
  dictHasBind :: DictHasBind c m
  dictHasBind = { bind }
