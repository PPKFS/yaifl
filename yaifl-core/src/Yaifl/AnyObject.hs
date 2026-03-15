{-|
Module      : Yaifl.AnyObject
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Type-safe polymorphic object representation.

This module provides a unified interface for working with objects that can be either
Things or Rooms in the game world. It enables type-safe operations on heterogeneous
object collections and provides conversion functions between specific and generic object types.

Key components:
- `AnyObject`: A type that can represent either a Thing or Room
- `CanBeAny`: Typeclass for converting between specific and generic object types
- Prisms: `_Room` and `_Thing` for pattern matching and extraction
- Utility functions: `unwrapAny`, `asThingOrRoom` for working with unified objects
-}

module Yaifl.AnyObject
  ( -- * Core Types
    IsObject(..)
  , CanBeAny(..)
  , AnyObject(..)
  
  -- * Prisms
  , _Room
  , _Thing
  
  -- * Type Aliases
  , TaggedAnyEnclosing
  , EnclosingThing
  
  -- * Utility Functions
  , asThingOrRoom
  , unwrapAny
  )
where


import Yaifl.Prelude


import GHC.Records
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.Thing.Kind
import Yaifl.Tag
import Yaifl.WorldModel

type RawAnyObject wm = Object wm (Either (ThingData wm) (RoomData wm)) (WMObjSpecifics wm)
-- | Either a room or a thing. The `Either` is over the object data so it's easier to
-- do things with the other fields.
--
-- This raw type is used internally by 'AnyObject' to represent objects that can be
-- either Things or Rooms while maintaining access to common fields like ID and specifics.
newtype AnyObject wm = AnyObject (RawAnyObject wm)
  deriving newtype (Eq, Ord, Generic)

instance HasField x (RawAnyObject wm) a => HasField x (AnyObject wm) a where
  getField (AnyObject o) = getField @x o

instance Display (WMText wm) => Display (AnyObject wm) where
  displayBuilder o = displayBuilder (o ^. #name)

instance HasEntity (AnyObject wm) where
  getEntity (AnyObject a) = objectId a

-- | Prism for extracting 'Room' values from 'AnyObject'.
--
-- This prism allows pattern matching on 'AnyObject' to extract 'Room' values,
-- and constructing 'AnyObject' values from 'Room' values.
--
-- Example usage:
-- @
-- preview _Room anyObj >>= \room -> doSomethingWith room
-- @
_Room :: Prism' (AnyObject wm) (Room wm)
_Room = prism' (AnyObject . first Right . coerce) (fmap Room . bitraverse rightToMaybe Just . (\(AnyObject a) -> a))

-- | Prism for extracting 'Thing' values from 'AnyObject'.
--
-- This prism allows pattern matching on 'AnyObject' to extract 'Thing' values,
-- and constructing 'AnyObject' values from 'Thing' values.
--
-- Example usage:
-- @
-- preview _Thing anyObj >>= \thing -> doSomethingWith thing
-- @
_Thing :: Prism' (AnyObject wm) (Thing wm)
_Thing = prism' (AnyObject . first Left . coerce) (fmap Thing . bitraverse leftToMaybe Just . (\(AnyObject a) -> a))

-- | Typeclass for converting between specific object types and 'AnyObject'.
--
-- This class provides bidirectional conversion between concrete object types (Room, Thing)
-- and the polymorphic 'AnyObject' type. The functional dependency @o -> wm@ ensures
-- that each object type is associated with exactly one world model.
--
-- Implementations should satisfy the round-trip property:
-- @fromAny (toAny x) == Just x@ for valid objects.
class CanBeAny wm o | o -> wm where
  -- | Convert a specific object type to 'AnyObject'.
  toAny :: o -> AnyObject wm
  
  -- | Convert 'AnyObject' back to a specific object type.
  -- Returns 'Nothing' if the object is not of the expected type.
  fromAny :: AnyObject wm -> Maybe o

instance CanBeAny wm (Room wm) where
  toAny = review _Room
  fromAny = preview _Room

instance CanBeAny wm (Thing wm) where
  toAny = review _Thing
  fromAny = preview _Thing

instance CanBeAny wm (AnyObject wm) where
  toAny = id
  fromAny = Just

instance IsObject (AnyObject wm) where
  isThing = isJust . fromAny @wm @(Thing wm)

type TaggedAnyEnclosing wm = TaggedObject (AnyObject wm) EnclosingTag

-- | Unwrap an 'AnyObject' into a tagged entity.
--
-- This function converts an 'AnyObject' into a tagged entity, preserving the object ID
-- and adding the appropriate tag (ThingTag or RoomTag) based on the object type.
--
-- This is useful when you need to work with the tagged entity representation
-- of an object for operations that require type discrimination.
unwrapAny ::
  AnyObject wm
  -> Either (TaggedEntity ThingTag) (TaggedEntity RoomTag)
unwrapAny a = case (preview _Thing a, preview _Room a) of
    (Just x, _) -> Left (tagEntity x (a ^. #objectId))
    (_, Just x) -> Right (tagEntity x (a ^. #objectId))
    _ -> error "impossible"

-- | Apply different functions based on whether the object is a Thing or Room.
--
-- This utility function eliminates boilerplate code for handling both object types.
-- It takes two functions (one for Things, one for Rooms) and applies the appropriate
-- one based on the actual type of the 'AnyObject'.
--
-- Example usage:
-- @
-- describeObject = asThingOrRoom describeThing describeRoom
-- @
asThingOrRoom ::
  (Thing wm -> a)
  -> (Room wm -> a)
  -> AnyObject wm
  -> a
asThingOrRoom tf rf a = case (preview _Thing a, preview _Room a) of
    (Just x, _) -> tf x
    (_, Just x) -> rf x
    _ -> error "impossible"