{-|
Module      : Yaifl.Object.Query
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Object query and modification utilities.
-}

module Yaifl.Object.Query
  (-- * Get
  getThingMaybe
  , getRoomMaybe
  -- * Modify
  , modifyObject
  , modifyThing
  , modifyRoom
  , isUnderstoodAs
  ) where

import Yaifl.Prelude

import Yaifl.Metadata
import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.ObjectLike
import Yaifl.Thing.Kind
import Yaifl.Room.Kind
import Yaifl.AnyObject

import qualified Data.Set as S
import Yaifl.Refreshable
import Yaifl.WorldModel

-- | Safely get a thing by its entity reference.
--
-- Returns `Nothing` if:
-- - The object doesn't exist
-- - The object exists but is not a thing
-- This is the safe way to access things when you're not certain
-- the object exists or is of the correct type.
getThingMaybe ::
  ObjectQuery wm :> es
  => Display (WMText wm)
  => WithMetadata es
  => ObjectLike wm o
  => o  -- ^ Object reference (entity, thing reference, etc.)
  -> Eff es (Maybe (Thing wm))  -- ^ Nothing if not found/wrong type, Just thing if found
getThingMaybe e = withoutMissingObjects (preview _Thing <$> getObject (getEntity e)) (const $ pure Nothing)

-- | Safely get a room by its entity reference.
--
-- Returns `Nothing` if:
-- - The object doesn't exist
-- - The object exists but is not a room
-- This is the safe way to access rooms when you're not certain
-- the object exists or is of the correct type.
getRoomMaybe ::
  ObjectQuery wm :> es
  => Display (WMText wm)
  => WithMetadata es
  => ObjectLike wm o
  => o  -- ^ Object reference (entity, room reference, etc.)
  -> Eff es (Maybe (Room wm))  -- ^ Nothing if not found/wrong type, Just room if found
getRoomMaybe e = withoutMissingObjects (preview _Room <$> getObject (getEntity e)) (const $ pure Nothing)

modifyObjectFrom ::
  WithMetadata es
  => (o -> Eff es (Object wm any s))
  -> (Object wm any s -> Eff es ())
  -> o
  -> (Object wm any s -> Object wm any s)
  -> Eff es ()
modifyObjectFrom g s o u = do
  obj <- g o
  let newObj = u obj
  ts <- getGlobalTime
  s (newObj { modifiedTime = ts})
  tickGlobalTime

-- | Modify a thing with type-safe access.
--
-- This function provides safe modification of thing objects with proper
-- error handling. The modification function receives the current thing
-- and returns a modified version.
modifyThing ::
  WithoutMissingObjects wm es
  => ThingLike wm o
  => o  -- ^ Thing reference (entity, thing, etc.)
  -> (Thing wm -> Thing wm)  -- ^ Modification function
  -> Eff es ()              -- ^ No return value (modification is in-place)
modifyThing o u = modifyObjectFrom (fmap coerce refreshThing) (setThing . Thing) o ((\(Thing a) -> a) . u . Thing)

-- | Modify a room with type-safe access.
--
-- This function provides safe modification of room objects with proper
-- error handling. The modification function receives the current room
-- and returns a modified version.
modifyRoom ::
  WithoutMissingObjects wm es
  => RoomLike wm o
  => o  -- ^ Room reference (entity, room, etc.)
  -> (Room wm -> Room wm)  -- ^ Modification function
  -> Eff es ()              -- ^ No return value (modification is in-place)
modifyRoom o u = modifyObjectFrom (fmap coerce refreshRoom) (setRoom . Room) o ((\(Room a) -> a) . u . Room)

-- | Modify any object with type-safe access.
--
-- This is a generic modification function that works with any object type
-- (things, rooms, etc.). It automatically dispatches to the appropriate
-- type-specific modification function based on the object's actual type.
modifyObject ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o  -- ^ Object reference (entity, thing, room, etc.)
  -> (AnyObject wm -> AnyObject wm)  -- ^ Modification function
  -> Eff es ()                         -- ^ No return value (modification is in-place)
modifyObject e s = do
  o <- getObject e
  asThingOrRoom
    (`modifyThing` anyModifyToThing s)
    (`modifyRoom` anyModifyToRoom s) o

anyModifyToThing ::
  (AnyObject s -> AnyObject s)
  -> (Thing s -> Thing s)
anyModifyToThing f t = fromMaybe t (preview _Thing $ f (review _Thing t))

anyModifyToRoom ::
  (AnyObject s -> AnyObject s)
  -> (Room s -> Room s)
anyModifyToRoom f t = fromMaybe t (preview _Room $ f (review _Room t))

-- | Add terms to an object's understanding list.
--
-- This function extends an object's metadata with additional terms that
-- the object can be understood or referred to as. This is used by the
-- parser to recognize different ways players might refer to objects.
--
-- The terms are added to the object's `understandAs` set in its metadata.
-- If a term is already present, it won't be duplicated.
isUnderstoodAs ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o  -- ^ Object reference to modify
  -> [Text]  -- ^ List of terms to add to understanding
  -> Eff es ()  -- ^ No return value (modification is in-place)
isUnderstoodAs o ls = do
  modifyObject o (#understandAs %~ S.union (S.fromList ls))
