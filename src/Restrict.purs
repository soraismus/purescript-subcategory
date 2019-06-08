module Control.Subcategory.Restrict
  ( class Restrict
  , restrict
  ) where

import Control.Category (identity) as Unrestricted
import Control.Subcategory.ObjectOf (class ObjectOf)
import Record.Builder (Builder)
import Unsafe.Coerce (unsafeCoerce)

class Restrict
  (c0 :: Type -> Type -> Type)
  (c1 :: Type -> Type -> Type)
  where
  restrict
    :: forall v0 v1
     . ObjectOf c1 v0
    => ObjectOf c1 v1
    => c0 v0 v1
    -> c1 v0 v1

instance restrictFnBuilder :: Restrict Function Builder where
  restrict
    :: forall v0 v1
     . ObjectOf Builder v0
    => ObjectOf Builder v1
    => (v0 -> v1)
    -> Builder v0 v1
  restrict = unsafeCoerce

instance restrictFnFn :: Restrict Function Function where
  restrict = Unrestricted.identity
