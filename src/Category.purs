module Control.Subcategory.Category
  ( class Category
  ) where

import Control.Subcategory.HasIdentity (class HasIdentity)
import Control.Subcategory.Semigroupoid (class Semigroupoid)
import Record.Builder (Builder)

class (HasIdentity c, Semigroupoid c) <= Category (c :: Type -> Type -> Type)

instance categoryFn :: Category Function

instance categoryBuilder :: Category Builder
