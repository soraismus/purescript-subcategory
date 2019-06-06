module Control.Restricted.Semigroupoid
  ( class Semigroupoid
  , compose        , (<<<)
  , composeFlipped , (>>>)
  ) where

import Control.Restricted.ObjectOf (class ObjectOf)
import Control.Semigroupoid (compose) as Unrestricted
import Record.Builder (Builder)

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
  compose = Unrestricted.compose

instance semigroupoidBuilder :: Semigroupoid Builder where
  compose = Unrestricted.compose
