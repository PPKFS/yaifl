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
-- Stores objects contained within an enclosing object (room, container, supporter).
-- Uses `EnumSet` for efficient storage and operations.
--
-- Capacity behavior:
-- - `Nothing`: Unlimited capacity
-- - `Just n`: Maximum of @n@ contained objects
--
-- Example:s
-- @
--   -- Infinite capacity container
--   bottomlessBag = Enclosing Data.EnumSet.empty Nothing
--
--   -- Limited capacity container (3 items max)
--   smallChest = Enclosing Data.EnumSet.empty (Just 3)
-- @
data Enclosing = Enclosing
  { contents :: EnumSet ThingEntity -- ^ The contained objects.
  , capacity :: Maybe Int -- ^ Optional maximum capacity. Nothing = unlimited.
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeFieldLabelsNoPrefix ''Enclosing

-- | Create an empty enclosing component.
blankEnclosing :: Enclosing
blankEnclosing = Enclosing
  { contents = Data.EnumSet.empty
  , capacity = Nothing
  }

-- | `Taggable` instance for `Enclosing`.
--
-- See `Yaifl.Tag` for more information on the tagging system.
instance Taggable Enclosing EnclosingTag