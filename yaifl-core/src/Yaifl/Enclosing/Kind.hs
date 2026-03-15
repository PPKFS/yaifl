{-|
Module      : Yaifl.Enclosing.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Enclosing components represent objects that can contain other objects,
including rooms, containers, and supporters.

This module defines the core `Enclosing` data structure and related types:

- `Enclosing`: The core component for object containment
- `EnclosingEntity`: Type-safe reference to enclosing objects
- Functions for creating and manipulating enclosing components

See also:
- `Yaifl.Object.Kind` for the base Object type
- `Yaifl.Thing.Kind` for thing containment
- `Yaifl.Room.Kind` for room enclosing functionality
-}

module Yaifl.Enclosing.Kind (
  -- * Enclosing types
    Enclosing(..)

  -- * Enclosing functions
  , blankEnclosing

  -- * Re-exports
  , EnclosingEntity
  ) where

import Yaifl.Prelude

import Data.EnumSet ( EnumSet, empty )
import Yaifl.Entity
import Yaifl.Tag

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