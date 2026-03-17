{-|
Module      : Yaifl.Store
Copyright   : (c) Avery 2023-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

Entity-indexed storage with optics support.

This module provides a specialised storage container for game entities that:

- Wraps `Data.EnumMap.EnumMap` for efficient `Entity`-keyed storage
- Provides `At` and `Ixed` instances for convenient lens-based access
- Supports monadic operations via `alterNewtypeEMF` helper
- Offers `Traversable` instance for bulk operations

The `Store` type is the primary mechanism for storing and retrieving game objects
by their `Entity` identifiers. It provides O(1) lookup, insertion, and deletion
while maintaining referential integrity.

Key features:
- **Type-safe**: Parameterised over the stored value type
- **Efficient**: Uses `EnumMap` internally for fast operations
- **Convenient**: Optics integration for ergonomic access patterns
- **Flexible**: Supports both pure and effectful operations

Example usage:
@
  -- Create a store and insert an object
  store <- pure (emptyStore :: Store Thing)
  let store' = store & at entityId ?~ thing

  -- Lookup an object
  case store' ^? at entityId of
    Just thing -> processThing thing
    Nothing -> handleMissingObject

  -- Modify an object
  let store'' = store' & at entityId %~ updateThing
@

This module is foundational for the game's object management system.
-}

module Yaifl.Store
  ( -- * Stores
    Store(..)
  , emptyStore
  -- * Internal utilities
  , alterEMF
  , alterNewtypeEMF
  ) where

import Yaifl.Prelude
import Yaifl.Entity ( Entity )
import qualified Data.EnumMap as EM
import qualified Data.IntMap as IM

-- | Entity-indexed storage container.
--
-- A newtype wrapper around `Data.EnumMap.EnumMap` that provides:
--
-- - O(1) lookup, insertion, and deletion by `Entity` keys
-- - Type-safe storage parameterised by the value type @a@
-- - Optics support via `At` and `Ixed` instances
-- - Efficient memory usage for sparse entity spaces
--
-- The `Store` is the primary data structure used throughout Yaifl for
-- managing game objects. It serves as the backing store for the game world's
-- entity-object mappings.
--
-- Example:
-- @
--   -- Store mapping entities to things
--   type ThingStore = Store Thing
--
--   -- Store mapping entities to rooms  
--   type RoomStore = Store Room
-- @
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  -- ^ Unwrap the store to access the underlying `EnumMap`
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Read, Foldable, Functor)

-- | Create an empty store.
--
-- This is the starting point for building entity stores. The empty store
-- contains no mappings and has O(1) space complexity.
--
-- Example:
-- @
--   -- Create an empty store for things
--   emptyThingStore = emptyStore :: Store Thing
-- @
emptyStore :: Store a
emptyStore = Store EM.empty

-- | Modify an `EnumMap` within a functorial context.
--
-- This helper function provides effectful modification of `EnumMap` values.
-- It converts the `EnumMap` to an `IntMap`, applies the modification function,
-- and converts back. This is used internally to implement the `At` instance.
--
-- Parameters:
-- - `upd`: Update function that transforms `Maybe a` to `f (Maybe a)`
-- - `k`: Key to modify
-- - `m`: EnumMap to modify
--
-- Returns: Modified EnumMap within the functor @f@
alterEMF ::
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))  -- ^ Update function
  -> k                          -- ^ Key to modify
  -> EM.EnumMap k a             -- ^ EnumMap to modify
  -> f (EM.EnumMap k a)         -- ^ Result in functor context
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

-- | Modify a newtype-wrapped `EnumMap` within a functorial context.
--
-- This is a generalised version of `alterEMF` that works with newtype wrappers.
-- It's used to implement the `At` instance for `Store` by handling the newtype
-- wrapping and unwrapping automatically.
--
-- Parameters:
-- - `upd`: Update function
-- - `k`: Key to modify
-- - `unwrap`: Function to extract `EnumMap` from newtype
-- - `wrap'`: Function to wrap `EnumMap` back into newtype
-- - `m`: Newtype-wrapped value to modify
--
-- Returns: Modified newtype-wrapped value within functor @f@
alterNewtypeEMF ::
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))  -- ^ Update function
  -> k                          -- ^ Key to modify
  -> (nt -> EM.EnumMap k a)      -- ^ Unwrap function
  -> (EM.EnumMap k a -> nt)      -- ^ Wrap function
  -> nt                         -- ^ Newtype to modify
  -> f nt                        -- ^ Result in functor context
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)

-- | `At` instance for `Store`.
--
-- This instance enables lens-based access to store elements using the `at` lens.
-- It provides convenient syntax for looking up, inserting, and modifying values:
--
-- @
--   -- Lookup: store ^? at entityId
--   -- Insert: store & at entityId ?~ value
--   -- Update: store & at entityId %~ f
--   -- Delete: store & at entityId .~ Nothing
-- @
--
-- The implementation uses `alterNewtypeEMF` to handle the newtype wrapping
-- and provide effectful operations when needed.
instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

-- | `Traversable` instance for `Store`.
--
-- This instance enables traversal of all values in the store, allowing
-- operations like sequencing effects or collecting results.
--
-- Example:
-- @
--   -- Sequence all actions in a store of IO values
--   results <- sequenceA (store :: Store (IO ()))
-- @
instance Traversable Store where
  sequenceA (Store s) = Store <$> sequenceA s

-- | `Ixed` instance for `Store`.
--
-- This instance enables indexed optics operations on stores. Combined with
-- the `At` instance, it provides comprehensive lens-based access patterns.
--
-- Type instances:
-- - `IxValue (Store a) = a`: The values stored in the store
-- - `Index (Store a) = Entity`: The entity keys used for indexing
--
-- This allows using indexed optics like `ix` for direct access:
--
-- @
--   store ^. ix entityId  -- Get value (with error if missing)
--   store & ix entityId .~ newValue  -- Set value
-- @
type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)