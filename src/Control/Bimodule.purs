module Control.Bimodule_
  ( class Bimodule_
      , assertContravCategory_
      , assertCovCategory_
      , assertTgtClosedMonoidal_
      , distribute_
  ) where

import Prelude (unit)

import Control.ObjectOf (class ObjectOf)
import Control.Category_ (class Category_)
import Record.Builder (Builder)

class Bimodule_ contrav cov tgt p where
  assertContravCategory_ :: Category_ contrav => Unit
  assertCovCategory_ :: Category_ cov => Unit
  assertTgtClosedMonoidal_ :: ClosedMonoidal_ tgt => Unit
  distribute_
    :: forall v0 v1 v2 v3
     . ObjectOf contrav v0
    => ObjectOf contrav v1
    => ObjectOf cov v2
    => ObjectOf cov v3
    => ObjectOf tgt (p v1 v2)
    => ObjectOf tgt (p v0 v3)
    => contrav v0 v1
    -> cov v2 v3
    -> tgt (p v1 v2) (p v0 v3)
