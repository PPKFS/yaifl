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

-- | Interface for types that can be 'refreshed' to ensure current data.
-- That is, we want to have actual resolved objects around (when running actions, for example)
-- but not for them to become out of sync with the current object model.
-- The 'WithoutMissingObjects' constraint ensures referenced objects exist.
class Refreshable wm av where
  refresh :: forall es. WithoutMissingObjects wm es => av -> Eff es av

instance {-# OVERLAPPING #-} (HasEntity o, Refreshable wm o) => Refreshable wm (TaggedObject o tagEntity) where
  -- | Refresh a tagged object by refreshing the underlying object and re-tagging.
  -- This preserves the tag while ensuring the object data is current.
  refresh obj = tagObject obj <$> refresh (getTaggedObject obj)

instance (Refreshable wm a, Refreshable wm b) => Refreshable wm (a, b) where
  -- | Refresh a pair by refreshing both elements.
  refresh (a, b) = refresh a >>= \a' -> refresh b <&> (a', )

instance Refreshable wm a => Refreshable wm (Store a) where
  -- | Refresh a store by refreshing all contained values.
  refresh = mapM refresh

instance Refreshable wm a => Refreshable wm (Maybe a) where
  -- | Refresh a Maybe value by refreshing the contained value if present.
  refresh t = case t of
    Nothing -> return t
    Just x -> Just <$> refresh x

instance (Refreshable wm b, Refreshable wm a) => Refreshable wm (Either a b) where
  -- | Refresh an Either value by refreshing whichever constructor is present.
  refresh = bimapM refresh refresh

instance Refreshable wm Int where
  -- | Int values are trivially refreshable (no state to update).
  refresh = pure

instance Refreshable wm (Thing wm) where
  -- | Refresh a thing using the specialized refreshThing function.
  refresh = refreshThing

instance Refreshable wm (AnyObject wm) where
  -- | Refresh an AnyObject by re-resolving its entity.
  refresh t = getObject (getEntity t)

instance Refreshable wm () where
  -- | Unit values are trivially refreshable (no state to update).
  refresh = const pass

instance Refreshable wm (Room wm) where
  -- | Refresh a room using the specialized refreshRoom function.
  refresh = refreshRoom

-- | Refresh a room to its current state.
--
-- Fetches latest room data and checks for modifications when tracing is enabled.
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
-- Fetches latest thing data and checks for modifications when tracing is enabled.
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
