module Control.Restricted.Category
  ( class Category
  ) where

import Control.Restricted.HasIdentity (class HasIdentity)
import Control.Restricted.Semigroupoid (class Semigroupoid)
import Record.Builder (Builder)

class (HasIdentity c, Semigroupoid c) <= Category (c :: Type -> Type -> Type)

instance categoryFn :: Category Function

instance categoryBuilder :: Category Builder
