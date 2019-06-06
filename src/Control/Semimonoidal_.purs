module Control.Semimonoidal_
  where

class Semimonoidal_
  (c :: Type -> Type -> Type)
  (t :: Type -> Type -> Type)
  where
  join_
    :: forall a b
     . ObjectOf c a
    => ObjectOf c b
    => ObjectOf c (t a b)
    => a
    -> b
    -> t a b
