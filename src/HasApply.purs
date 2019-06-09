module Control.Subcategory.HasApply
  ( class HasApply
  , apply
  , applyFirst
  , applySecond
  , lift2
  , lift3
  , lift4
  , lift5
  ) where

import Control.Apply (class Apply, apply) as Unrestricted
import Control.Subcategory.Constituency (class ObjectOf)
import Control.Subcategory.HasConst (class HasConst, const)
import Control.Subcategory.HasIdentity (class HasIdentity, identity)
import Control.Subcategory.Endofunctor.HasMap (class HasMap, map)
import Control.Subcategory.Restrictable (class Restrictable, restrict)
import Control.Subcategory.Slackable (class Slackable, slacken)

-- | The `HasApply` class provides the `(<*>)` which is used to apply a
-- | function to an argument under a type constructor.
-- |
-- | `HasApply` can be used to lift functions of two or more arguments to work
-- | on values wrapped with the type constructor `f`. It might also be
-- | understood in terms of the `lift2` function:
-- |
-- | ```purescript
-- | lift2 :: forall f a b c. HasApply f => (a -> b -> c) -> f a -> f b -> f c
-- | lift2 f a b = f <$> a <*> b
-- | ```
-- |
-- | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
-- | the function application operator `($)` to arguments wrapped with the
-- | type constructor `f`.
class HasApply c f where
  apply
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (c v0 v1)
    => f (c v0 v1)
    -> c (f v0) (f v1)

applyFirst
  :: forall c f v0 v1
   . HasApply c f
  => HasConst c
  => HasMap c f
  -- -------
  -- constX0
  -- -------
  => ObjectOf c v0                       -- #0
  => ObjectOf c v1                       -- #1
  => ObjectOf c (c v1 v0)                -- #2
  => ObjectOf c (c v0 (c v1 v0))         -- #3
  -- -------
  -- APPLY f
  -- -------
  => ObjectOf c (f (c v1 v0))            -- #4. #2
  => ObjectOf c (f (c v0 (c v1 v0)))     -- #5. #3
  -- ------
  -- OUTPUT
  -- ------
  => ObjectOf c (f v0)                   -- #6
  => ObjectOf c (f v1)                   -- #7
  => ObjectOf c (c (f v1) (f v0))        -- #8
  -- -------------
  -- INVERSE apply
  -- -------------
  => ObjectOf c (c (f v1) (f v0))        -- #9.  #4
  => ObjectOf c (c (f v0) (f (c v1 v0))) -- #10. #5
  -- -----------------
  => Restrictable Function c
  => Slackable c
  => f v0
  -> c (f v1) (f v0)
applyFirst fx0 =
    restrict \fx1 -> slacken (apply constX0) fx1
  where
  constX0 :: f (c v1 v0)
  constX0 = slacken (map const) fx0

applySecond
  :: forall c f v0 v1
   . HasApply c f
  => HasConst c
  => HasIdentity c
  => HasMap c f
  -- -------------
  -- constIdentity
  -- -------------
  => ObjectOf c (c (c v1 v1) (c v0 (c v1 v1))) -- #0
  -- -----------------
  -- evalConstIdentity
  -- -----------------
  => ObjectOf c v0                             -- #1
  => ObjectOf c v1                             -- #2
  => ObjectOf c (c v1 v1)                      -- #3
  => ObjectOf c (c v0 (c v1 v1))               -- #4
  -- -------
  -- APPLY f
  -- -------
  => ObjectOf c (f (c v1 v1))                  -- #5. #3
  => ObjectOf c (f (c v0 (c v1 v1)))           -- #6. #4
  -- ------
  -- OUTPUT
  -- ------
  => ObjectOf c (f v0)                         -- #7
  => ObjectOf c (f v1)                         -- #8
  => ObjectOf c (c (f v1) (f v1))              -- #9
  -- -------------
  -- INVERSE apply
  -- -------------
  => ObjectOf c (c (f v0) (f (c v1 v1)))       -- #10. #6
  -- -------------
  => Restrictable Function c
  => Slackable c
  => f v0
  -> c (f v1) (f v1)
applySecond x0 = restrict \x1 ->
    slacken (apply (slacken (map evalConstIdentity) x0)) x1
  where
  evalConstIdentity :: c v0 (c v1 v1)
  evalConstIdentity = slacken const identity

lift2
  :: forall c f v0 v1 v2
   . HasApply c f
  => HasMap c f
  -- -----
  -- INPUT
  -- -----
  => ObjectOf c v0                       -- #0
  => ObjectOf c v1                       -- #1
  => ObjectOf c v2                       -- #2
  => ObjectOf c (c v1 v2)                -- #3
  -- -------
  -- APPLY f
  -- -------
  => ObjectOf c (f (c v1 v2))            -- #4. #3
  -- ------
  -- OUTPUT
  -- ------
  => ObjectOf c (f v0)                   -- #5
  => ObjectOf c (f v1)                   -- #6
  => ObjectOf c (f v2)                   -- #7
  => ObjectOf c (c (f v1) (f v2))        -- #8
  => ObjectOf c (c (f v0) (f (c v1 v2))) -- #9
  -- -----------
  -- APPLY apply
  -- -----------
  => ObjectOf c (f (c v0 (c v1 v2)))     -- #10. #9
  -- -----------
  => Restrictable Function c
  => Slackable c
  => c v0 (c v1 v2)
  -> c (f v0) (c (f v1) (f v2))
lift2 f = restrict \x0 -> restrict \x1 ->
  slacken (apply (slacken (map f) x0)) x1

lift3
  :: forall c f v0 v1 v2 v3
   . HasApply c f
  => HasMap c f
  -- -----
  -- INPUT
  -- -----
  => ObjectOf c v0                                      -- #0
  => ObjectOf c v1                                      -- #1
  => ObjectOf c v2                                      -- #2
  => ObjectOf c v3                                      -- #3
  => ObjectOf c (c v2 v3)                               -- #4
  => ObjectOf c (c v1 (c v2 v3))                        -- #5
  -- -------
  -- APPLY f
  -- -------
  => ObjectOf c (f (c v2 v3))                           -- #5. #4
  => ObjectOf c (f (c v1 (c v2 v3)))                    -- #6. #5
  -- ------
  -- OUTPUT
  -- ------
  => ObjectOf c (f v0)                                  -- #7
  => ObjectOf c (f v1)                                  -- #8
  => ObjectOf c (f v2)                                  -- #9
  => ObjectOf c (f v3)                                  -- #10
  => ObjectOf c (c (f v2) (f v3))                       -- #11
  => ObjectOf c (c (f v1) (c (f v2) (f v3)))            -- #12
  => ObjectOf c (c (f v0) (c (f v1) (c (f v2) (f v3)))) -- #13
  -- -----------
  -- APPLY apply
  -- -----------
  => ObjectOf c (c (f v1) (f (c v2 v3)))                -- #15. #12
  => ObjectOf c (c (f v0) (c (f v1) (f (c v2 v3))))     -- #17. #13
  => ObjectOf c (c (f v0) (f (c v1 (c v2 v3))))         -- #18. #17
  -- -----------
  => Restrictable Function c
  => Slackable c
  => c v0 (c v1 (c v2 v3))
  -> c (f v0) (c (f v1) (c (f v2) (f v3)))
lift3 f =
  restrict \x0 ->
    restrict \x1 ->
      restrict \x2 ->
        slacken (apply (
          slacken (apply (
            slacken (map f) x0)) x1)) x2

lift4
  :: forall c f v0 v1 v2 v3 v4
   . HasApply c f
  => HasMap c f
  -- -----
  -- INPUT
  -- -----
  => ObjectOf c v0                                                 -- #0
  => ObjectOf c v1                                                 -- #1
  => ObjectOf c v2                                                 -- #2
  => ObjectOf c v3                                                 -- #3
  => ObjectOf c v4                                                 -- #4
  => ObjectOf c (c v3 v4)                                          -- #5
  => ObjectOf c (c v2 (c v3 v4))                                   -- #6
  => ObjectOf c (c v1 (c v2 (c v3 v4)))                            -- #7
  -- -------
  -- APPLY f
  -- -------
  => ObjectOf c (f (c v3 v4))                                      -- #16. #5
  => ObjectOf c (f (c v2 (c v3 v4)))                               -- #17. #6
  => ObjectOf c (f (c v1 (c v2 (c v3 v4))))                        -- #18. #7
  -- ------
  -- OUTPUT
  -- ------
  => ObjectOf c (f v0)                                             -- #8
  => ObjectOf c (f v1)                                             -- #9
  => ObjectOf c (f v2)                                             -- #10
  => ObjectOf c (f v3)                                             -- #11
  => ObjectOf c (f v4)                                             -- #11
  => ObjectOf c (c (f v3) (f v4))                                  -- #12
  => ObjectOf c (c (f v2) (c (f v3) (f v4)))                       -- #13
  => ObjectOf c (c (f v1) (c (f v2) (c (f v3) (f v4))))            -- #14
  => ObjectOf c (c (f v0) (c (f v1) (c (f v2) (c (f v3) (f v4))))) -- #15
  -- -----------
  -- APPLY apply
  -- -----------
  => ObjectOf c (c (f v2) (f (c v3 v4)))                           -- #19. #13
  => ObjectOf c (c (f v1) (c (f v2) (f (c v3 v4))))                -- #20. #14
  => ObjectOf c (c (f v1) (f (c v2 (c v3 v4))))                    -- #21. #20
  => ObjectOf c (c (f v0) (c (f v1) (c (f v2) (f (c v3 v4)))))     -- #22. #15
  => ObjectOf c (c (f v0) (c (f v1) (f (c v2 (c v3 v4)))))         -- #23. #22
  => ObjectOf c (c (f v0) (f (c v1 (c v2 (c v3 v4)))))             -- #24. #23
  -- -----------
  => Restrictable Function c
  => Slackable c
  => c v0 (c v1 (c v2 (c v3 v4)))
  -> c (f v0) (c (f v1) (c (f v2) (c (f v3) (f v4))))
lift4 f =
  restrict \x0 ->
    restrict \x1 ->
      restrict \x2 ->
        restrict \x3 ->
          slacken (apply (
            slacken (apply (
              slacken (apply (
                slacken (map f) x0)) x1)) x2)) x3

lift5
  :: forall c f v0 v1 v2 v3 v4 v5
   . HasApply c f
  => HasMap c f
  -- -----
  -- INPUT
  -- -----
  => ObjectOf c v0                                                            -- #0
  => ObjectOf c v1                                                            -- #1
  => ObjectOf c v2                                                            -- #2
  => ObjectOf c v3                                                            -- #3
  => ObjectOf c v4                                                            -- #4
  => ObjectOf c v5                                                            -- #4
  => ObjectOf c (c v4 v5)                                                     -- #5
  => ObjectOf c (c v3 (c v4 v5))                                              -- #6
  => ObjectOf c (c v2 (c v3 (c v4 v5)))                                       -- #7
  => ObjectOf c (c v1 (c v2 (c v3 (c v4 v5))))                                -- #8
  -- -------
  -- APPLY f
  -- -------
  => ObjectOf c (f (c v4 v5))                                                 -- #9.  #5
  => ObjectOf c (f (c v3 (c v4 v5)))                                          -- #10. #6
  => ObjectOf c (f (c v2 (c v3 (c v4 v5))))                                   -- #11. #7
  => ObjectOf c (f (c v1 (c v2 (c v3 (c v4 v5)))))                            -- #12. #8
  -- ------
  -- OUTPUT
  -- ------
  => ObjectOf c (f v0)                                                        -- #13
  => ObjectOf c (f v1)                                                        -- #14
  => ObjectOf c (f v2)                                                        -- #15
  => ObjectOf c (f v3)                                                        -- #16
  => ObjectOf c (f v4)                                                        -- #17
  => ObjectOf c (f v5)                                                        -- #18
  => ObjectOf c (c (f v4) (f v5))                                             -- #19
  => ObjectOf c (c (f v3) (c (f v4) (f v5)))                                  -- #20
  => ObjectOf c (c (f v2) (c (f v3) (c (f v4) (f v5))))                       -- #21
  => ObjectOf c (c (f v1) (c (f v2) (c (f v3) (c (f v4) (f v5)))))            -- #22
  => ObjectOf c (c (f v0) (c (f v1) (c (f v2) (c (f v3) (c (f v4) (f v5)))))) -- #23
  -- -----------
  -- APPLY apply
  -- -----------
  => ObjectOf c (c (f v3) (f (c v4 v5)))                                      -- #24. #20
  => ObjectOf c (f (c v3 (c v4 v5)))                                          -- #25. #24
  => ObjectOf c (c (f v2) (c (f v3) (f (c v4 v5))))                           -- #26. #21
  => ObjectOf c (c (f v2) (f (c v3 (c v4 v5))))                               -- #27. #26
  => ObjectOf c (c (f v1) (c (f v2) (c (f v3) (f (c v4 v5)))))                -- #28. #22
  => ObjectOf c (c (f v1) (c (f v2) (f (c v3 (c v4 v5)))))                    -- #29. #28
  => ObjectOf c (c (f v1) (f (c v2 (c v3 (c v4 v5)))))                        -- #30. #29
  => ObjectOf c (c (f v0) (c (f v1) (c (f v2) (c (f v3) (f (c v4 v5))))))     -- #31. #23
  => ObjectOf c (c (f v0) (c (f v1) (c (f v2) (f (c v3 (c v4 v5))))))         -- #32. #31
  => ObjectOf c (c (f v0) (c (f v1) (f (c v2 (c v3 (c v4 v5))))))             -- #33. #32
  => ObjectOf c (c (f v0) (f (c v1 (c v2 (c v3 (c v4 v5))))))                 -- #34. #33
  -- -----------
  => Restrictable Function c
  => Slackable c
  => c v0 (c v1 (c v2 (c v3 (c v4 v5))))
  -> c (f v0) (c (f v1) (c (f v2) (c (f v3) (c (f v4) (f v5)))))
lift5 f =
  restrict \x0 ->
    restrict \x1 ->
      restrict \x2 ->
        restrict \x3 ->
          restrict \x4 ->
            slacken (apply (
              slacken (apply (
                slacken (apply (
                  slacken (apply (
                    slacken (map f) x0)) x1)) x2)) x3)) x4

instance applyUnrestricted :: Unrestricted.Apply f => HasApply Function f where
  apply = Unrestricted.apply






--
-- if c := (->)
--
--    f (c0 v0 v1)            -> c1 (f v0) (f v1)
--    |
--    | uncurry c1
--    v
-- t1 (f (c0 v0 v1)) (f v0)   ->           f v1
--    |
--    | semimonoidal f
--    v
-- f (t1 (c0 v0 v1) v0)       ->           f v1
--    |
--    | map eval 1/0?     [map (slacken eval) -- slacken isn't necessary]
--    v
-- f (v1)                    ->           f v1
--
--
-- HasApply maps from category c to category (->)
--   so currying and uncurrying are possible
--
--         eval :: forall v0 v1.   c (t (c v0 v1) v0)      v1
-- slacken eval :: forall v0 v1.      t (c v0 v1) v0  ->   v1
-- map     eval :: forall f v0 v1. f (t (c v0 v1) v0) -> f v1


-- apply'
--   :: forall c f v0 v1
--    . HasApply c f
--   => Slackable c
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => ObjectOf c (c v0 v1)
--   => Restrictable Function c
--   => f (c v0 v1)
--   -> f v0
--   -> f v1
-- -- apply' = lift2 (restrict slacken)
-- -- apply' = lift2 (restrict \v0 -> slacken slacken v0)
-- apply' ff fx0
--
-- slacken' :: c v0 v1 -> v1
-- slacken' f = slacken f x0
-- slacken'' :: c (c v0 v1) v1
-- slacken'' = restrict slacken'
--
-- ($) :: forall a b. (a -> b) -> a -> b
-- eval
--   => (c v0 v1)
--   -> v0
--   -> v1
-- lift2
--   => c v0 (c v1 v2)
--   -> f v0
--   -> f v1
--   -> f v2
