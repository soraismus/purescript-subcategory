module Control.Restricted.ClosedMonoidal
  ( class ClosedSemimonoidal
  , class HasCurriedEval
  , class HasCurry
  , class HasUncurriedEval
  , class HasUncurry
  , curriedEval
  , curry
  , eval
  , uncurriedEval
  , uncurry
  ) where

import Prelude ((<<<))

import Control.Restricted.HasUnit (class HasUnit)
import Control.Restricted.ObjectOf (class ObjectOf)
import Data.Function (flip) as Function
import Data.Tuple (Tuple)
import Data.Tuple (curry, swap, uncurry) as Tuple

class Semimonoidal
  (c         :: Type -> Type -> Type)
  (bifunctor :: Type -> Type -> Type)
  (tensor    :: Type -> Type -> Type)
  (f         :: Type -> Type)
  where
  join
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c (tensor v0 v1)
    => bifunctor (f v0) (f v1)
    -> f (tensor v0 v1)

class
  ( HasUnit c u1
  , ObjectOf c u1
  )
  <= HasExtrinsicUnit
      (c  :: Type -> Type -> Type)
      (f  :: Type -> Type)
      (u0 :: Type)
      (u1 :: Type)
      where
      extrinsicUnit :: ObjectOf c u1 => u0 -> f u1

class
  ( Semimonoidal c bifunctor tensor f
  , HasExtrinsicUnit c f u0 u1
  )
  <= Monoidal c bifunctor tensor f u0 u1

class HasCurry
  (c      :: Type -> Type -> Type)
  (tensor :: Type -> Type -> Type)
  (exp    :: Type -> Type -> Type)
  where
  curry
    :: forall v0 v1 v2
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => ObjectOf c (tensor v0 v1)
    => ObjectOf c (exp v0 v2)
    => c (tensor v0 v1) v2
    -> c v1 (exp v0 v2)

instance hasCurryFn :: HasCurry Function Tuple Function where
  curry = Function.flip <<< Tuple.curry

class HasUncurry
  (c       :: Type -> Type -> Type)
  (tensor :: Type -> Type -> Type)
  (exp    :: Type -> Type -> Type)
  where
  uncurry
    :: forall v0 v1 v2
     . ObjectOf c v0
    => ObjectOf c v1
    => ObjectOf c v2
    => ObjectOf c (tensor v0 v1)
    => ObjectOf c (exp v0 v2)
    => c v1 (exp v0 v2)
    -> c (tensor v0 v1) v2

instance hasUncurryFn :: HasUncurry Function Tuple Function where
  uncurry f = Tuple.uncurry f <<< Tuple.swap

class
  ( HasCurry c tensor exp
  , HasUncurry c tensor exp
  )
    <= ClosedSemimonoidal c tensor exp

class HasCurriedEval
  (c       :: Type -> Type -> Type)
  (exp    :: Type -> Type -> Type)
  where
  curriedEval
    :: forall v0 v1
     . ObjectOf c (exp v0 v1)
    => c (exp v0 v1) (exp v0 v1)

class HasUncurriedEval
  (c      :: Type -> Type -> Type)
  (tensor :: Type -> Type -> Type)
  (exp    :: Type -> Type -> Type)
  where
  uncurriedEval
    :: forall v0 v1
     . ObjectOf c (tensor (exp v0 v1) v0)
    => ObjectOf c v1
    => c (tensor (exp v0 v1) v0) v1

eval
  :: forall exp c v0 v1
   . HasCurriedEval c exp
  => ObjectOf c (exp v0 v1)
  => c (exp v0 v1) (exp v0 v1)
eval = curriedEval
