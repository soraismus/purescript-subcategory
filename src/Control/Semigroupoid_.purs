module Control.Semigroupoid_
  ( class Semigroupoid_
  , compose_        , (<<<)
  , composeFlipped_ , (>>>)
  )
  where

import Control.Semigroupoid (compose)

import Control.ObjectOf (class ObjectOf)

-- | `p` is a sub-semigroupoid of PureScript's canonical semigroupoid `Function`.
class Semigroupoid_ p where
  compose_
    :: forall a b c
     . ObjectOf p a
    => ObjectOf p b
    => ObjectOf p c
    => p b c
    -> p a b
    -> p a c

infixr 9 compose_ as <<<

-- | Forwards composition, or `compose` with its arguments reversed.
composeFlipped_ :: forall a b c d. Semigroupoid a => a b c -> a c d -> a b d
composeFlipped_ f g = compose g f

infixr 9 composeFlipped_ as >>>

instance semigroupoid_Fn :: Semigroupoid_ Function where
  compose_ = compose

instance semigroupoid_Builder :: Semigroupoid_ Builder where
  compose_ = compose
