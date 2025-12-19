{-|
Module      : Yaifl.ObjectLike
Copyright   : (c) Avery 2022-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

Typeclasses for things which are XLike (can be resolved into an X in an @Eff es@ context with relevant
constraints/effects).
-}

module Yaifl.ObjectLike
  ( ObjectLike(..)
  , ThingLike(..)
  , RoomLike(..)
  , objectIsKind
  ) where

import Yaifl.Prelude

import Effectful.Error.Static

import Yaifl.AnyObject
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Metadata ( isKind, Metadata, WithMetadata )
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.Tag
import Yaifl.Thing.Kind

-- | Something which can be resolved into an `AnyObject`.
class HasID o => ObjectLike wm o where
  getObject :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (AnyObject wm)

-- | Something which can be resolved into a `Thing`.
class HasID o => ThingLike wm o where
  getThing :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (Thing wm)

-- | Something which can be resolved into a `Room`.
class HasID o => RoomLike wm o where
  getRoom :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (Room wm)

instance (ObjectLike wm o) => ObjectLike wm (TaggedObject o tagEntity) where
  getObject = getObject . unTagObject

instance {-# OVERLAPPABLE #-} (RoomLike wm o) => RoomLike wm (TaggedObject o tagEntity) where
  getRoom = getRoom . snd . unTagObject

instance {-# OVERLAPPABLE #-} (ThingLike wm o) => ThingLike wm (TaggedObject o tagEntity) where
  getThing = getThing . snd . unTagObject

instance ObjectLike wm (Thing wm) where
  getObject = pure . toAny

instance ObjectLike wm (Room wm) where
  getObject = pure . toAny

instance ThingLike wm (Thing wm) where
  getThing = pure

instance RoomLike wm (Room wm) where
  getRoom = pure

instance ThingLike wm (TaggedEntity ThingTag) where
  getThing o = fromMaybe (error $ "tagged (thing) entity could not resolve " <> show o) . preview _Thing <$> getObject (unTagEntity o)

instance RoomLike wm (TaggedEntity RoomTag) where
  getRoom o = fromMaybe (error $ "tagged (room) entity could not resolve " <> show o) . preview _Room <$> getObject (unTagEntity o)

instance ObjectLike wm (AnyObject wm) where
  getObject = pure

instance ObjectLike wm (TaggedEntity anyTag) where
  getObject e = getObject (unTagEntity e)

instance ThingLike wm DoorEntity where
  getThing = getThing . coerceTag @ThingTag

instance ThingLike wm PersonEntity where
  getThing = getThing . coerceTag @ThingTag

instance ObjectLike wm Entity where
  getObject e = if isThing (getID e)
    then lookupThing e >>= either (throwError . flip MissingObject e) (return . review _Thing)
    else lookupRoom e >>= either (throwError . flip MissingObject e) (return . review _Room)

instance ObjectLike wm o => ObjectLike wm (TaggedEntity e, o) where
  getObject = getObject . snd

instance ThingLike wm (TaggedObject (Thing wm) o) where
  getThing = pure . snd . unTagObject

instance RoomLike wm (TaggedObject (Room wm) o) where
  getRoom = pure . snd . unTagObject

objectIsKind ::
  WithoutMissingObjects wm es
  => WithMetadata es
  => ObjectLike wm o
  => ObjectKind
  -> o
  -> Eff es Bool
objectIsKind t o = getObject o >>= (`isKind` t)