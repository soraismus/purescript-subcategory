module Control.Subcategories.Category
  ( class Category
  , assertIdentity
  , assertSemigroupoid
  )
  where

import Prelude (Unit, unit)

import Control.Subcategories.Identity (class Identity)
import Control.Subcategories.Semigroupoid (class Semigroupoid)
import Record.Builder (Builder)

class Category (p :: Type -> Type -> Type) where
  assertIdentity :: Identity p => Unit
  assertSemigroupoid :: Semigroupoid p => Unit

instance categoryFn :: Category Function where
  assertIdentity = unit
  assertSemigroupoid = unit

instance categoryBuilder :: Category Builder where
  assertIdentity = unit
  assertSemigroupoid = unit
