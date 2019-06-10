module Control.Subcategory.Endofunctor.Monad
  ( class Monad
  ) where

import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.Endofunctor.Applicative (class Applicative)
import Control.Subcategory.Endofunctor.Bind (class Bind)
import Control.Subcategory.Endofunctor.HasBind (class HasBind, bindFlipped)
import Control.Subcategory.Endofunctor.HasPure (class HasPure, pure')
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken)
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
ap mf = f9
  where
  f9 :: c (m v0) (m v1)
  f9 = restrict f8
  f8 :: m v0 -> m v1 -- closes over `mf`
  f8 mv0 = f7 mf
    where
    f7 :: m (c v0 v1) -> m v1
    f7 = slacken f6
    f6 :: c (m (c v0 v1)) (m v1)
    f6 = bindFlipped f5
    f5 :: c (c v0 v1) (m v1)
    f5 = restrict f4
    f4 :: c v0 v1 -> m v1 -- closes over `mv0`
    f4 f = f3 mv0
      where
      f3 :: m v0 -> m v1
      f3 = slacken f2
      f2 :: c (m v0) (m v1)
      f2 = bindFlipped f1
      f1 :: c v0 (m v1)
      f1 = restrict f0
      f0 :: v0 -> m v1 -- closes over `f`
      f0 v0 = x1
        where
        fa :: c v1 (m v1)
        fa = pure' (Proxy3 :: Proxy3 c)
        fb :: v1 -> m v1
        fb = slacken fa

        fc :: v0 -> v1
        fc = slacken f

        x0 :: v1
        x0 = fc v0

        x1 :: m v1
        x1 = fb x0



--       f0 :: v0 -> m v1 -- closes over `f`
--       f0 v0 = slacken (pure' (Proxy3 :: Proxy3 c)) (f v0)


-- ap mf = f9
--   where
--   f0 :: v0 -> m v1 -- closes over `f`
--   f0 = \v0 -> slacken (pure' (Proxy3 :: Proxy3 c)) (f v0)
--   f1 :: c v0 (m v1)
--   f1 = restrict f0
--   f2 :: c (m v0) (m v1)
--   f2 = bindFlipped f1
--   f3 :: m v0 -> m v1
--   f3 = slacken f2
--   f4 :: c v0 v1 -> m v1 -- closes over `mv0`
--   f4 = \f -> f3 mv0
--   f5 :: c (c v0 v1) (m v1)
--   f5 = restrict f4
--   f6 :: c (m (c v0 v1)) (m v1)
--   f6 = bindFlipped f5
--   f7 :: m (c v0 v1) -> m v1
--   f7 = slacken f6
--   f8 :: m v0 -> m v1 -- closes over `mf`
--   f8 = \mv0 -> f7 mf
--   f9 :: c (m v0) (m v1)
--   f9 = restrict f8

-- ap mf =
--   restrict
--     (\mv0 ->
--       (slacken
--         (bindFlipped
--           (restrict
--             (\f ->
--                 (slacken
--                   (bindFlipped
--                     (restrict (\v0 -> slacken (pure' (Proxy3 :: Proxy3 c)) (f v0)))
--                   )
--                   mv0
--                 )
--             )
--           )
--         )
--         mf
--       )
--     )

-- ap mf mv0 = do
--   f' <- mf
--   v0' <- mv0
--   pure (f' v0')
--
-- ap mf mv0 =
--   mf >>= \f' -> mv0 >>= \v0' -> pure (f' v0')
--
-- ap mf mv0 =
--   bindFlipped
--     (\f' -> mv0 >>= \v0' -> pure (f' v0'))
--     mf
--
-- ap mf mv0 =
--   bindFlipped
--     (\f' ->
--               bindFlipped
--                 (\v0' -> pure (f' v0'))
--                 mv0
--     )
--     mf
--
-- ap f =
--     bindFlipped x1
--   where
--   c = Proxy3 :: Proxy3 c
--   x0 :: c v0 v1 -> v0 -> m v1
--   x0 = \f' -> \v0' -> slacken (pure' c) (slacken f' v0)
-- --   x1 :: c v0 v1 -> c (m v0) (m v1)
-- --   x1 = \f' -> bindFlipped (restrict (x0 f'))
--   x2 :: c v0 v1 -> m v0 -> m v1
--   x2 = \f' -> \mv0' -> slacken (bindFlipped (restrict (x0 f'))) mv0'


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
