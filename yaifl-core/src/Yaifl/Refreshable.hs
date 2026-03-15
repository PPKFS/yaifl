{-|
Module      : Yaifl.Refreshable
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Object refresh system for data consistency.

This module provides a typeclass and functions for refreshing objects to ensure they
contain current data from the game state. The refresh system handles cases where
objects may have been modified or become stale.

Key components:
- `Refreshable`: Typeclass defining the refresh interface
- `refresh`: Main refresh operation that updates objects to current state
- `refreshRoom`: Refresh operation specifically for rooms
- `refreshThing`: Refresh operation specifically for things
-}

module Yaifl.Refreshable
  ( -- * Core Typeclass
    Refreshable(..)
  
  -- * Refresh Functions
  , refreshRoom
  , refreshThing
  )
where


import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.Thing.Kind
import Yaifl.Metadata
import Yaifl.ObjectLike
import Yaifl.Store
import Yaifl.Tag
import Data.Bitraversable

-- | Typeclass for refreshable values.
--
-- This class defines the interface for types that can be refreshed to ensure they
-- contain current data. Implementations should fetch the latest version of the
-- value and return it.
--
-- The 'WithoutMissingObjects' constraint ensures that refresh operations can assume
-- referenced objects exist, simplifying error handling.
class Refreshable wm av where
  -- | Refresh a value to its current state.
  --
  -- This method updates the value to reflect the latest data from the game state.
  -- For objects, this typically involves re-fetching from the store.
  --
  -- @wm@: The world model type
  -- @av@: The value type being refreshed
  -- @es@: Effect stack requiring 'WithoutMissingObjects wm'
  --
  -- Returns the refreshed value.
  refresh :: forall es. WithoutMissingObjects wm es => av -> Eff es av

instance {-# OVERLAPPING #-} (HasEntity o, Refreshable wm o) => Refreshable wm (TaggedObject o tagEntity) where
  refresh obj = tagObject obj <$> refresh (getTaggedObject obj)

instance (Refreshable wm a, Refreshable wm b) => Refreshable wm (a, b) where
  refresh (a, b) = refresh a >>= \a' -> refresh b <&> (a', )

instance Refreshable wm a => Refreshable wm (Store a) where
  refresh = mapM refresh

instance Refreshable wm a => Refreshable wm (Maybe a) where
  refresh t = case t of
    Nothing -> return t
    Just x -> Just <$> refresh x

instance (Refreshable wm b, Refreshable wm a) => Refreshable wm (Either a b) where
  refresh = bimapM refresh refresh

instance Refreshable wm Int where
  refresh = pure

instance Refreshable wm (Thing wm) where
  refresh = refreshThing

instance Refreshable wm (AnyObject wm) where
  refresh t = getObject (getEntity t)

instance Refreshable wm () where
  refresh = const pass

instance Refreshable wm (Room wm) where
  refresh = refreshRoom

-- | Refresh a room to its current state.
--
-- This function fetches the latest version of a room from the object store and checks
-- if it has been modified since the last refresh. If modification is detected,
-- a runtime error is noted for debugging purposes.
refreshRoom ::
  WithoutMissingObjects wm es
  => RoomLike wm o
  => o -- ^ Room-like object to refresh (Room or RoomEntity)
  -> Eff es (Room wm)
refreshRoom tl = do
  r <- getRoom tl
  ifM (traceGuard Medium)
    (do
      r'' <- getRoom (tagRoomEntity r)
      when ((r'' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed room with ID" <> show (display $ view #name r) <> " and found an outdated object"
      return r'')
    (pure r)

-- | Refresh a thing to its current state.
--
-- This function fetches the latest version of a thing from the object store and checks
-- if it has been modified since the last refresh. If modification is detected,
-- a runtime error is noted for debugging purposes.
refreshThing ::
  WithoutMissingObjects wm es
  => ThingLike wm o
  => o -- ^ Thing-like object to refresh (Thing or ThingEntity)
  -> Eff es (Thing wm)
refreshThing tl = do
  r <- getThing tl
  ifM (traceGuard Medium)
    (do
      r'' <- getThing (tagThingEntity r)
      when ((r'' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed thing with ID" <> show (display $ view #name r) <> " and found an outdated object"
      return r'')
    (pure r)
