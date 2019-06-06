module Control.Restricted.HasMap
  ( class HasMap
  , map        , (<$>)
  , mapFlipped , (<#>)
  ) where

import Control.Restricted.Eval (class Eval, eval)
import Control.Restricted.HasDimap (class HasDimap, arr)
import Control.Restricted.HasIdentity (class HasIdentity, identity)
import Control.Restricted.ObjectOf (class ObjectOf)
import Control.Restricted.Restrict (class Restrict, restrict)
import Data.Functor (class Functor, map) as Unrestricted

class HasMap
  (c :: Type -> Type -> Type)
  (f :: Type -> Type)
  where
  map
    :: forall v0 v1
     . ObjectOf c v0
    => ObjectOf c v1
    => c v0 v1
    -> f v0
    -> f v1

infixl 4 map as <$>

mapFlipped
  :: forall c f v0 v1
   . HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => f v0
  -> c v0 v1
  -> f v1
mapFlipped fa f = f <$> fa

infixl 1 mapFlipped as <#>

-- void
--   :: forall c f u v
--    . HasMap c f
--   => HasConst c
--   => HasUnit c u
--   => ObjectOf c v
--   => f v
--   -> f u
-- void = map (const unit)

-- voidLeft
--   :: forall c f v0 v1
--    . HasConst c
--   => HasMap c f
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => f v0
--   -> v1
--   -> f v1
-- voidLeft f x = const x <$> f
-- infixl 4 voidLeft as $>

-- voidRight
--   :: forall c f v0 v1
--    . HasConst c
--   => HasMap c f
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => v0
--   -> f v1
--   -> f v0
-- voidRight x = map (const x)
-- infixl 4 voidRight as <$

-- -- | Apply a value in a computational context to a value in no context.
-- -- |
-- -- | Generalizes `flip`.
-- -- |
-- -- | ```purescript
-- -- | longEnough :: String -> Bool
-- -- | hasSymbol :: String -> Bool
-- -- | hasDigit :: String -> Bool
-- -- | password :: String
-- -- |
-- -- | validate :: String -> Array Bool
-- -- | validate = flap [longEnough, hasSymbol, hasDigit]
-- -- | ```
-- -- |
-- -- | ```purescript
-- -- | flap (-) 3 4 == 1
-- -- | threeve <$> Just 1 <@> 'a' <*> Just true == Just (threeve 1 'a' true)
-- -- | ```
-- flap :: forall f a b. Functor f => f (a -> b) -> a -> f b
-- flap ff x = map (\f -> f x) ff
--
-- infixl 4 flap as <@>

type HasMap_ c f =
  { map
      :: forall v0 v1
       . HasMap c f
      => ObjectOf c v0
      => ObjectOf c v1
      => c v0 v1
      -> f v0
      -> f v1
  }

-- flap
--   :: forall c f p v0 v1
--    . HasDimap c p
--   => Eval c
--   => HasIdentity p
--   => HasMap c f
--   => ObjectOf c v0
--   => ObjectOf c v1
--   => ObjectOf c (c v0 v1)
--   => Restrict Function c
--   => f (c v0 v1)
--   -> v0
--   -> f v1
-- flap ff x =
--   map (restrict (\f -> eval f x)) ff

flap
  :: forall c f p v0 v1
   . HasDimap c p
  => Eval c
  => HasIdentity p
  => HasMap c f
  => ObjectOf c v0
  => ObjectOf c v1
  => ObjectOf c (c v0 v1)
  => Restrict Function c
  => f (c v0 v1)
  -> v0
  -> f v1
flap ff x =
  map' (restrict (\f -> eval f x)) ff
  where
  dictionary :: HasMap_ c f
  dictionary = { map: map }
  map' = dictionary.map

instance functorUnrestricted
  :: Unrestricted.Functor f
  => HasMap Function f
  where
  map = Unrestricted.map
