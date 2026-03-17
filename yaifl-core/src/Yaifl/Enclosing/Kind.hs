{-|
Module      : Yaifl.Enclosing.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Object containment and spatial relationships.

This module defines the core data structures for representing objects that can
contain other objects. In Yaifl, "enclosing" is a fundamental concept that enables:

- **Containment**: Objects that can hold other objects (containers, rooms, supporters)
- **Spatial relationships**: Parent-child relationships between game objects
- **Capacity management**: Limits on how many objects can be contained
- **Content tracking**: Efficient storage and retrieval of contained objects

The `Enclosing` component is used by various object types:

- **Rooms**: Contain things, people, and other objects
- **Containers**: Boxes, chests, bags that hold portable items
- **Supporters**: Surfaces like tables that can have objects on them

Key features:
- **Efficient storage**: Uses `EnumSet` for compact representation of contents
- **Capacity control**: Optional limits on contained objects
- **Type safety**: Tagged entities for safe references
- **Extensible**: Designed to work with Yaifl's component system

Example usage:
@
  -- Create a container with limited capacity
  chestEnclosing = blankEnclosing { capacity = Just 5 }

  -- Add an object to a container
  let updatedEnclosing = chestEnclosing { contents = contents chestEnclosing <> one thingEntity }

  -- Check if a container is full
  isFull enclosing = case capacity enclosing of
    Just limit -> size (contents enclosing) >= limit
    Nothing -> False
@

This module works closely with:
- `Yaifl.Object.Kind` for base object functionality
- `Yaifl.Thing.Kind` for portable object containment
- `Yaifl.Room.Kind` for location-based enclosing
- `Yaifl.Container.Kind` and `Yaifl.Supporter.Kind` for specific enclosing types
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