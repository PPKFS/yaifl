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
  , EnclosingEntity

  -- * Enclosing functions
  , blankEnclosing

  -- * Re-exports
  , module Yaifl.Tag
  ) where

import Yaifl.Prelude

import Data.EnumSet ( EnumSet, empty )
import Yaifl.Entity
import Yaifl.Tag

-- | Component representing an object's containment capabilities.
--
-- This data type stores the core information needed for an object to contain
-- other objects. It's used as a component in Yaifl's entity-component system.
--
-- Fields:
-- - `contents`: Set of thing entities currently contained (uses `EnumSet` for efficiency)
-- - `capacity`: Optional maximum number of items that can be contained
--
-- Capacity semantics:
-- - `Nothing`: Infinite capacity (unlimited containment)
-- - `Just n`: Can contain at most @n@ objects
--
-- Example:
-- @
--   -- A bottomless bag (infinite capacity)
--   bottomlessBag = Enclosing Data.EnumSet.empty Nothing
--
--   -- A small chest (limited to 3 items)
--   smallChest = Enclosing Data.EnumSet.empty (Just 3)
-- @
--
-- Performance characteristics:
-- - `contents` lookup: O(1)
-- - `contents` insertion: O(1)
-- - `contents` size: O(1)
-- - Memory: Compact `EnumSet` representation
data Enclosing = Enclosing
  { contents :: EnumSet ThingEntity -- ^ The contained objects.
  , capacity :: Maybe Int -- ^ An optional number of items that can be contained. Nothing = infinite.
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeFieldLabelsNoPrefix ''Enclosing

-- | Create an empty enclosing component.
--
-- This creates a new `Enclosing` component with:
-- - Empty contents (no contained objects)
-- - Unlimited capacity (can contain any number of objects)
--
-- This is the starting point for creating containers, rooms, and supporters.
-- You can then modify the `contents` and `capacity` fields as needed.
--
-- Example:
-- @
--   -- Create a basic room enclosure
--   roomEnclosing = blankEnclosing
--
--   -- Create a limited container
--   chestEnclosing = blankEnclosing { capacity = Just 10 }
-- @
blankEnclosing :: Enclosing
blankEnclosing = Enclosing
  { contents = Data.EnumSet.empty
  , capacity = Nothing
  }

-- | `Taggable` instance for `Enclosing`.
--
-- This instance enables type-safe tagging of enclosing components using
-- the `EnclosingTag` phantom type. It's used throughout Yaifl for safe
-- references to enclosing objects.
--
-- See `Yaifl.Tag` for more information on the tagging system.
instance Taggable Enclosing EnclosingTag