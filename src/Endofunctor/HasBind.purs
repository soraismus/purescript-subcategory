module Control.Subcategory.Endofunctor.HasBind
  ( class HasBind
  , bindFlipped           , (=<<)
--   , composeKleisli        , (>=>)
--   , composeKleisliFlipped , (<=<)
--   , ifM
--   , join
  ) where

import Prelude (($))

import Control.Bind (class Bind, bindFlipped) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.HasPure (class HasPure, pure')
import Control.Subcategory.HasIdentity (class HasIdentity, identity)
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken)
import Data.Function (flip)
import Type.Proxy (Proxy3(Proxy3))

class HasBind c m where
  bindFlipped
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c (m v1)
    => c v0 (m v1)
    -> c (m v0) (m v1)

infixr 1 bindFlipped as =<<

-- bind'
--   :: forall c m v0 v1
--    . HasBind c m
--   => ObjectOf c v0
--   => ObjectOf c (m v1)
--   => Restrictable Function c
--   => m v0
--   -> (v0 -> (m v1))
--   -> m v1
-- bind' mx mf = bindX (restrict mf)
--   where
--   bindX :: c v0 (m v1) -> m v1
--   bindX = bind mx
--
-- infixl 1 bind as >>>=

instance bindUnrestricted
  :: Unrestricted.Bind m
  => HasBind Function m
  where
  bindFlipped = Unrestricted.bindFlipped

join
  :: forall c m v
   . HasBind c m
  => HasIdentity c
  => HasPure c m
  => ObjectOf c v
  => ObjectOf c (m v)
  => ObjectOf c (m (m v))
  => c (m (m v)) (m v)
join = result
  where
        x0 :: c v (m v)
        x0 = pure' (Proxy3 :: Proxy3 c)
        x1 :: c (m v) (m v)
        x1 = bindFlipped x0
        result :: c (m (m v)) (m v)
        result = bindFlipped x1

-- bindFlipped :: c v0 (m v1) -> c (m v0) (m v1)
-- bindFlipped :: c (m (m v)) (m v) -> c (m (m (m v))) (m v)






-- composeKleisli
--   :: forall c m v0 v1 v2
--    . HasBind c m
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => ObjectOf c (m v1)
--   => ObjectOf c (m v2)
--   => ObjectOf c (c v0 (m v1))
--   => Slackable c
--   => c v0 (m v1)
--   -> c v1 (m v2)
--   -> v0
--   -> m v2
-- composeKleisli f g a = slacken f a >>= g
--
-- infixr 1 composeKleisli as >=>
--
-- composeKleisliFlipped
--   :: forall c m v0 v1 v2
--    . HasBind c m
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => ObjectOf c (m v1)
--   => ObjectOf c (m v2)
--   => ObjectOf c (c v0 (m v1))
--   => Slackable c
--   => c v1 (m v2)
--   -> c v0 (m v1)
--   -> v0
--   -> m v2
-- composeKleisliFlipped f g a = f =<< slacken g a
--
-- infixr 1 composeKleisliFlipped as <=<
--
-- ifM
--   :: forall c m v
--    . HasBind c m
--   => ObjectOf c Boolean
--   => ObjectOf c (m v)
--   => Restrictable Function c
--   => m Boolean
--   -> m v
--   -> m v
--   -> m v
-- ifM mCond mt mf =
--     bindCond $ restrict (if _ then mt else mf)
--   where
--   bindCond :: c Boolean (m v) -> m v
--   bindCond = bind mCond
