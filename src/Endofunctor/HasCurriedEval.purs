module Control.Subcategory.HasCurriedEval
  ( class HasCurriedEval
  , curriedEval
  , eval
  ) where

import Control.Subcategory.Constituency (class ObjectOf)

class HasCurriedEval
  (c   :: Type -> Type -> Type)
  (exp :: Type -> Type -> Type)
  where
  curriedEval
    :: forall v0 v1
     . ObjectOf c (exp v0 v1)
    => c (exp v0 v1) (exp v0 v1)

eval
  :: forall c exp v0 v1
   . HasCurriedEval c exp
  => ObjectOf c (exp v0 v1)
  => c (exp v0 v1) (exp v0 v1)
eval = curriedEval
