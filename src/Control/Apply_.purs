module Control.Apply_
  ( class Apply_
  , apply_, (<*>)
  , applyFirst_, (<*)
  , applySecond_, (*>)
  , lift2_
  , lift3_
  , lift4_
  , lift5_
  ) where

import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))
import Data.Function (const)
import Control.Category (identity)

class Apply_ c f where
  apply_
    :: forall a b
     . ObjectOf c a
    => ObjectOf c b
    => ObjectOf c (c a b)
    => f (c a b)
    -> f a
    -> f b
  assertFunctor_ :: Functor_ c f => Unit
  assertSemimonoidal_ :: Semimonoidal_ c Tuple => Unit

infixl 4 apply as <*>

instance applyFn :: Apply ((->) r) where
  apply f g x = f x (g x)

instance applyArray :: Apply Array where
  apply = arrayApply

foreign import arrayApply :: forall a b. Array (a -> b) -> Array a -> Array b

applyFirst :: forall a b f. Apply f => f a -> f b -> f a
applyFirst a b = const <$> a <*> b

infixl 4 applyFirst as <*

applySecond :: forall a b f. Apply f => f a -> f b -> f b
applySecond a b = const identity <$> a <*> b

infixl 4 applySecond as *>

lift2 :: forall a b c f. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b

lift3 :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 f a b c = f <$> a <*> b <*> c

lift4 :: forall a b c d e f. Apply f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
lift4 f a b c d = f <$> a <*> b <*> c <*> d

lift5 :: forall a b c d e f g. Apply f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
lift5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
