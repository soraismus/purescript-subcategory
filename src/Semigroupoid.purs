module Control.Subcategory.Semigroupoid
  ( class Semigroupoid
  , compose        , (<<<)
  , composeFlipped , (>>>)
  )
  where

import Control.Subcategory.ObjectOf (class ObjectOf)
import Control.Semigroupoid (compose) as Super
import Record.Builder (Builder)

-- | `s` is a sub-semigroupoid of PureScript's canonical semigroupoid `Function`.
class Semigroupoid s where
  compose
    :: forall v0 v1 v2
     . ObjectOf s v0
    => ObjectOf s v1
    => ObjectOf s v2
    => s v1 v2
    -> s v0 v1
    -> s v0 v2

infixr 9 compose as <<<

-- | Forwards composition, or `compose` with its arguments reversed.
composeFlipped
  :: forall s v0 v1 v2
   . ObjectOf s v0
  => ObjectOf s v1
  => ObjectOf s v2
  => Semigroupoid s
  => s v0 v1
  -> s v1 v2
  -> s v0 v2
composeFlipped f g = compose g f

infixr 9 composeFlipped as >>>

instance semigroupoidFn :: Semigroupoid Function where
  compose = Super.compose

instance semigroupoidBuilder :: Semigroupoid Builder where
  compose = Super.compose
