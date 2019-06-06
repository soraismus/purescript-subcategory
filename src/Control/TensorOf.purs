module Control.TensorOf
  where

import Data.Either (Either)
import Data.Tuple (Tuple)

class TensorOf
  (c :: Type -> Type -> Type)
  (t :: Type -> Type -> Type)

instance tensorOfFunctionTuple :: TensorOf Function Tuple
instance tensorOfFunctionEither :: TensorOf Function Either
