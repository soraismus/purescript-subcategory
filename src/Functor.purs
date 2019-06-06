module Control.Restricted.Functor
  ( class Functor
--   , flap       , (<@>)
  , map        , (<$>)
  , mapFlipped , (<#>)
--   , void
--   , voidLeft   , ($>)
--   , voidRight  , (<$)
  ) where

import Control.Restricted.Category (class Category)
-- import Control.Restricted.HasConst (class HasConst, const)
-- import Control.Restricted.HasUnit (class HasUnit, unit)
import Control.Restricted.ObjectOf (class ObjectOf)
-- import Control.Restricted.Semigroupoid ((>>>))

import Data.Functor (class Functor, map) as Unrestricted
-- import Data.Newtype (class Newtype, wrap, unwrap)
-- import Data.Profunctor (class Profunctor, dimap) as Unrestricted

class
  Category c
    <= Functor
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
   . Functor c f
  => ObjectOf c v0
  => ObjectOf c v1
  => f v0
  -> c v0 v1
  -> f v1
mapFlipped fa f = f <$> fa

infixl 1 mapFlipped as <#>

-- void
--   :: forall c f u v
--    . HasConst c
--   => Functor c f
--   => HasUnit c u
--   => ObjectOf c v
--   => f v
--   -> f u
-- void = map (const unit)

-- voidLeft
--   :: forall c f v0 v1
--    . HasConst c
--   => Functor c f
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
--   => Functor c f
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

instance functorUnrestricted
  :: Unrestricted.Functor f
  => Functor Function f
  where
  map = Unrestricted.map
