module Control.Profunctor__
  ( class Profunctor__
  , assertSrcCategory_
  , assertTgtCategory_
  , dimap__
  ) where

import Prelude (unit)

import Control.ObjectOf (class ObjectOf)
import Control.Category_ (class Category_)
import Record.Builder (Builder)

class Profunctor__ contrav cov p where
  assertContravCategory_ :: Category_ contrav => Unit
  assertCovCategory_ :: Category_ cov => Unit
  dimap__
    :: forall v0 v1 v2 v3
     . ObjectOf contrav v0
    => ObjectOf contrav v1
    => ObjectOf cov v2
    => ObjectOf cov v3
    => contrav v0 v1
    -> cov v2 v3
    -> p v1 v2
    -> p v0 v3

-- => Functor__ cov contrav f
-- => Profunctor__ contrav cov (Compose Function f)
-- => convtrav v0 v1
-- -> cov v2 v3
-- -> f v1 v2
-- -> f v0 v2
