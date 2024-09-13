{-|
Module      : Yaifl.Model.Objects.ObjectLike
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Typeclasses for things which are XLike (can be resolved into an X in an @Eff es@ context with relevant
constraints/effects).
-}

module Yaifl.Model.ObjectLike
  ( ObjectLike(..)
  , ThingLike(..)
  , RoomLike(..)
  , objectIsKind
  ) where

import Yaifl.Prelude

import Effectful.Error.Static

import Yaifl.Model.Kinds.Object
import Yaifl.Model.Effects
import Yaifl.Model.Entity
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Room
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Metadata (isKind)

-- | Something which can be resolved into an `AnyObject`.
class HasID o => ObjectLike wm o where
  getObject :: NoMissingRead wm es => o -> Eff es (AnyObject wm)

-- | Something which can be resolved into a `Thing`.
class HasID o => ThingLike wm o where
  getThing :: NoMissingRead wm es => o -> Eff es (Thing wm)

-- | Something which can be resolved into a `Room`.
class HasID o => RoomLike wm o where
  getRoom :: NoMissingRead wm es => o -> Eff es (Room wm)

instance (ObjectLike wm o) => ObjectLike wm (TaggedObject o tag) where
  getObject = getObject . unTagObject

instance (RoomLike wm o) => RoomLike wm (TaggedObject o tag) where
  getRoom = getRoom . snd . unTagObject

instance (ThingLike wm o) => ThingLike wm (TaggedObject o tag) where
  getThing = getThing . snd . unTagObject

instance ObjectLike wm (Thing wm) where
  getObject = pure . toAny

instance ObjectLike wm (Room wm) where
  getObject = pure . toAny

instance ThingLike wm (Thing wm) where
  getThing = pure

instance RoomLike wm (Room wm) where
  getRoom = pure

instance ObjectLike wm (AnyObject wm) where
  getObject = pure

instance ObjectLike wm (TaggedEntity anyTag) where
  getObject e = getObject (unTag e)

instance ThingLike wm DoorEntity where
  getThing = getThing . (toTag @_ @ThingTag)

instance ObjectLike wm Entity where
  getObject e = if isThing (getID e)
    then lookupThing e >>= either (throwError . flip MissingObject e) (return . review _Thing)
    else lookupRoom e >>= either (throwError . flip MissingObject e) (return . review _Room)

instance ObjectLike wm o => ObjectLike wm (TaggedEntity e, o) where
  getObject = getObject . snd

instance ThingLike wm (TaggedEntity ThingTag) where
  getThing o = fromMaybe (error $ "taggedentity could not resolve " <> show o) . preview _Thing <$> getObject (unTag o)

instance RoomLike wm (TaggedEntity RoomTag) where
  getRoom o = fromMaybe (error $ "taggedentity could not resolve " <> show o) . preview _Room <$> getObject (unTag o)

objectIsKind ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => ObjectKind
  -> o
  -> Eff es Bool
objectIsKind t o = getObject o >>= (`isKind` t)