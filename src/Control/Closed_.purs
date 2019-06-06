module Control.Closed_
  ( class Closed_
  , assertExponential
  )

class Closed_ (c :: Type -> Type -> Type) where
  assertExponential :: forall a b. ObjectOf c (c a b) => Unit

instance closed_Function Function where
  assertExponential = unit
