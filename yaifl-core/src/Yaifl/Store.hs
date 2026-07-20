{-|
Module      : Yaifl.Store
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Entity-indexed storage with optics support.

Provides a specialised storage container for game entities that:
- Wraps `Data.EnumMap.EnumMap` for efficient `Entity`-keyed storage
- Provides `At` and `Ixed` instances for convenient lens-based access
- Supports monadic operations via `alterNewtypeEMF` helper
- Offers `Traversable` instance for bulk operations

The `Store` type is the primary mechanism for storing and retrieving game objects
by their `Entity` identifiers with O(1) operations.
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
-- - O(1) lookup, insertion, and deletion by `Entity` keys
-- - Type-safe storage parameterised by the value type @a@
-- - Optics support via `At` and `Ixed` instances
-- - Efficient memory usage for sparse entity spaces
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  -- ^ Unwrap the store to access the underlying `EnumMap`
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Read, Foldable, Functor)

-- | Create an empty store.
--
-- Starting point for building entity stores. Contains no mappings, O(1) space.
emptyStore :: Store a
emptyStore = Store EM.empty

-- | Modify an `EnumMap` within a functorial context.
--
-- Helper function for effectful modification of `EnumMap` values.
-- Converts `EnumMap` to `IntMap`, applies modification, and converts back.
alterEMF ::
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))  -- ^ Update function
  -> k                          -- ^ Key to modify
  -> EM.EnumMap k a             -- ^ EnumMap to modify
  -> f (EM.EnumMap k a)         -- ^ Result in functor context
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

-- | Modify a newtype-wrapped `EnumMap` within a functorial context.
--
-- Generalised version of `alterEMF` that works with newtype wrappers.
-- Handles newtype wrapping/unwrapping automatically.
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
-- Enables lens-based access using the `at` lens for lookup, insertion, modification, and deletion.
instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

-- | `Traversable` instance for `Store`.
--
-- Enables traversal of all values in the store for operations like sequencing effects.
instance Traversable Store where
  sequenceA (Store s) = Store <$> sequenceA s

-- | `Ixed` instance for `Store`.
--
-- Enables indexed optics operations on stores with `IxValue (Store a) = a` and `Index (Store a) = Entity`.
type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)