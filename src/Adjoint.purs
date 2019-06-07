module Control.Restricted.Adjoint
  ( class Adjoint
  , class HasToLeft
  , class HasToRight
  , toLeft
  , toRight
  )
  where

import Control.Restricted.ClosedMonoidal
  ( class HasCurry
  , class HasUncurry
  , curry
  , uncurry
  )
import Control.Restricted.ObjectOf (class ObjectOf)

class HasToLeft
  (c0 :: Type -> Type -> Type)
  (c1 :: Type -> Type -> Type)
  (l  :: Type -> Type)
  (r  :: Type -> Type)
  where
  toLeft
    :: forall v0 v1
     . ObjectOf c0 (l v0)
    => ObjectOf c0 v1
    => ObjectOf c1 v0
    => ObjectOf c1 (r v1)
    => c1 v0 (r v1)
    -> c0 (l v0) v1

class HasToRight
  (c0 :: Type -> Type -> Type)
  (c1 :: Type -> Type -> Type)
  (l  :: Type -> Type)
  (r  :: Type -> Type)
  where
  toRight
    :: forall v0 v1
     . ObjectOf c0 (l v0)
    => ObjectOf c0 v1
    => ObjectOf c1 v0
    => ObjectOf c1 (r v1)
    => c0 (l v0) v1
    -> c1 v0 (r v1)

class (HasToLeft c0 c1 l r, HasToRight c0 c1 l r) <= Adjoint c0 c1 l r

instance hasToLeftHasUncurry
  :: ( HasUncurry c tensor exp
     , ObjectOf c v
     )
  => HasToLeft c c (tensor v) (exp v)
  where
  toLeft = uncurry

instance hasToRightHasCurry
  :: ( HasCurry c tensor exp
     , ObjectOf c v
     )
  => HasToRight c c (tensor v) (exp v)
  where
  toRight = curry
