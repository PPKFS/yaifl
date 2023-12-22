{-|
Module      : Yaifl.Model.Properties.Enclosing
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

A property component for things that can contain other things (rooms, supporters, containers, etc).
-}

module Yaifl.Model.Properties.Enclosing (
  -- * Enclosing
    Enclosing(..)
  , blankEnclosing
  ) where

import Solitude

import Data.EnumSet ( EnumSet, empty )
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Objects.Tag

-- | A component that contains other objects.
data Enclosing = Enclosing
  { contents :: EnumSet ThingEntity -- ^ The contained objects.
  , capacity :: Maybe Int -- ^ An optional number of items that can be contained. Nothing = infinite.
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeFieldLabelsNoPrefix ''Enclosing

-- | An enclosing component with nothing in it.
blankEnclosing :: Enclosing
blankEnclosing = Enclosing
  { contents = Data.EnumSet.empty
  , capacity = Nothing
  }

instance Taggable Enclosing EnclosingTag