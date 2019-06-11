module Control.Subcategory.Endofunctor.Monad
  ( class Monad
  ) where

import Prelude (($))

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.Applicative (class Applicative)
import Control.Subcategory.Endofunctor.Bind (class Bind)
import Control.Subcategory.Endofunctor.HasBind (class HasBind, bindFlipped)
import Control.Subcategory.Endofunctor.HasPure (class HasPure, pure')
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken, slacken')
import Type.Proxy (Proxy3(Proxy3))

class (Applicative c m, Bind c m) <= Monad c m

instance monad :: (Applicative c m, Bind c m) => Monad c m

ap
  :: forall c m v0 v1
   . HasBind c m
  => HasPure c m
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (m v0)
  => ObjectOf c (m v1)
  => ObjectOf c (c v0 v1)
  => ObjectOf c (m (c v0 v1))
  => Restrictable Function c
  => Slackable c
  => m (c v0 v1)
  -> c (m v0) (m v1)
ap mf =
  restrict \mv0 ->
    callOn mf  $ slacken' c $ bindFlipped $ restrict \f ->
    callOn mv0 $ slacken' c $ bindFlipped $ restrict \v0 ->
    slacken (pure' c) $ slacken f v0
  where
  c = Proxy3 :: Proxy3 c

callOn :: forall v0 v1. v0 -> (v0 -> v1) -> v1
callOn x f = f x

-- ap mf = g0
--   where
--   g0 :: c (m v0) (m v1)
--   g0 = restrict g1
--   g1 :: m v0 -> m v1 -- closes over `mf`
--   g1 mv0 = g2 mf
--     where
--     g2 :: m (c v0 v1) -> m v1
--     g2 = slacken g3
--     g3 :: c (m (c v0 v1)) (m v1)
--     g3 = bindFlipped g4
--     g4 :: c (c v0 v1) (m v1)
--     g4 = restrict g5
--     g5 :: c v0 v1 -> m v1 -- closes over `mv0`
--     g5 f = g6 mv0
--       where
--       g6 :: m v0 -> m v1
--       g6 = slacken g7
--       g7 :: c (m v0) (m v1)
--       g7 = bindFlipped g8
--       g8 :: c v0 (m v1)
--       g8 = restrict g9
--       g9 :: v0 -> m v1 -- closes over `f`
--       g9 v0 = x1
--         where
--         g10 :: c v1 (m v1)
--         g10 = pure' (Proxy3 :: Proxy3 c)
--         g11 :: v1 -> m v1
--         g11 = slacken g10
--         g12 :: v0 -> v1
--         g12 = slacken f
--         x0 :: v1
--         x0 = g12 v0
--         x1 :: m v1
--         x1 = g11 x0


-- liftM1 :: forall m v0 v1. Monad m => (v0 -> v1) -> m v0 -> m v1
-- liftM1 f v0 = do
--   v0' <- v0
--   pure (f v0')
--
-- whenM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
-- whenM mv1 m = do
--   v1 <- mv1
--   when v1 m
--
-- unlessM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
-- unlessM mv1 m =  do
--   v1 <- mv1
--   unless v1 m
