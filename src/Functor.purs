module Control.Subcategory.Functor
  ( class Functor
  ) where

import Control.Subcategory.Category (class Category)
import Control.Subcategory.HasMap (class HasMap)
import Record.Builder (Builder)

class (Category c, HasMap c f) <= Functor c f

instance functorFunction :: HasMap Function f => Functor Function f

instance functorBuilder :: HasMap Builder f => Functor Builder f
