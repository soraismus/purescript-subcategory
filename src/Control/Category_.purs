module Control.Category_
  ( class Category_
    , assertSemigroupoid_
    , assertIdentity_
  )
  where

import Prelude (unit)

import Control.Identity_ (class Identity_)
import Control.Semigroupoid_ (class Semigroupoid_)
import Record.Builder (Builder)

class Category_ (p :: Type -> Type -> Type) where
  assertSemigroupoid_ :: Semigroupoid_ p => Unit
  assertIdentity_ :: Identity_ p => Unit

instance category_Fn :: Category_ Function where
  assertSemigroupoid_ = unit
  assertIdentity_ = unit

instance category_Builder :: Category_ Builder where
  assertSemigroupoid_ = unit
  assertIdentity_ = unit
