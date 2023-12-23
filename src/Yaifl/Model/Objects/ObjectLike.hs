module Yaifl.Model.Objects.ObjectLike
  ( ObjectLike(..)
  , ThingLike(..)
  , RoomLike(..)
  ) where

import Solitude
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Object
import Effectful.Error.Static

class HasID o => ObjectLike wm o where
  getObject :: NoMissingRead wm es => o -> Eff es (AnyObject wm)

class HasID o => ThingLike wm o where
  getThing :: NoMissingRead wm es => o -> Eff es (Thing wm)

class HasID o => RoomLike wm o where
  getRoom :: NoMissingRead wm es => o -> Eff es (Room wm)

instance (ObjectLike wm o) => ObjectLike wm (TaggedObject o tag) where
  getObject = getObject . unTagObject

instance (RoomLike wm o) => RoomLike wm (TaggedObject o tag) where
  getRoom = getRoom . unTagObject

instance (ThingLike wm o) => ThingLike wm (TaggedObject o tag) where
  getThing = getThing . unTagObject

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
