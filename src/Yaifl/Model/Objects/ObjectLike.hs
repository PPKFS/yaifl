{-# LANGUAGE DefaultSignatures #-}
module Yaifl.Model.Objects.ObjectLike
  ( ObjectLike(..)

  ) where

import Solitude
import Yaifl.Model.Entity
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Object
import Effectful.Error.Static

class HasID o => ObjectLike wm o where
  getRoom :: NoMissingRead wm es => o -> Eff es (Room wm)
  default getRoom :: NoMissingRead wm es => o -> Eff es (Room wm)
  getRoom o = throwError $ MissingObject "called getRoom on an object with no instance"  (getID o)

  getThing :: NoMissingRead wm es => o -> Eff es (Thing wm)
  default getThing :: NoMissingRead wm es => o -> Eff es (Thing wm)
  getThing o = throwError $ MissingObject "called getThing on an object with no instance"  (getID o)

instance ObjectLike wm (Thing wm) where
  getThing = pure

instance ObjectLike wm (Room wm) where
  getRoom = pure

instance ObjectLike wm (AnyObject wm) where
  getThing t = either throwError pure
    (maybeToRight (MissingObject ("Tried to get a thing from " <> show (t ^. #objectId) <> " but it was a room.") (getID t))
      (preview _Thing t))
  getRoom t = either throwError pure
    (maybeToRight (MissingObject ("Tried to get a room from " <> show (t ^. #objectId) <> " but it was a thing.") (getID t))
      (preview _Room t))

instance ObjectLike wm Entity where
  getRoom e = lookupRoom e >>= either (throwError . flip MissingObject e) return
  getThing e = lookupThing e >>= either (throwError . flip MissingObject e) return