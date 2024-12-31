module Yaifl.Core.Query.Object
  ( -- * Types

  -- * Get
  getThingMaybe
  , getRoomMaybe
  -- * Modify
  , modifyObject
  , modifyThing
  , modifyRoom
  , getCurrentPlayer
  , isUnderstoodAs

  , makeItScenery
  ) where

import Yaifl.Prelude
import Breadcrumbs

import Yaifl.Core.Metadata
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.ObjectLike
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Kinds.AnyObject

import qualified Data.Set as S
import Yaifl.Core.Refreshable

getThingMaybe ::
  ObjectLookup wm :> es
  => WithMetadata es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Thing wm))
getThingMaybe e = withoutMissingObjects (preview _Thing <$> getObject (getID e)) (const $ pure Nothing)

getRoomMaybe ::
  ObjectLookup wm :> es
  => WithMetadata es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Room wm))
getRoomMaybe e = withoutMissingObjects (preview _Room <$> getObject (getID e)) (const $ pure Nothing)

modifyObjectFrom ::
  State Metadata :> es
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

modifyThing ::
  NoMissingObjects wm es
  => ThingLike wm o
  => o
  -> (Thing wm -> Thing wm)
  -> Eff es ()
modifyThing o u = modifyObjectFrom (fmap coerce refreshThing) (setThing . Thing) o ((\(Thing a) -> a) . u . Thing)

modifyRoom ::
  NoMissingObjects wm es
  => RoomLike wm o
  => o
  -> (Room wm -> Room wm)
  -> Eff es ()
modifyRoom o u = modifyObjectFrom (fmap coerce refreshRoom) (setRoom . Room) o ((\(Room a) -> a) . u . Room)

modifyObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> Eff es ()
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

getCurrentPlayer ::
  Breadcrumbs :> es
  => ObjectLookup wm :> es
  => State Metadata :> es
  => Eff es (Thing wm)
getCurrentPlayer = failHorriblyIfMissing $ use #currentPlayer >>= getThing

isUnderstoodAs ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> [Text]
  -> Eff es ()
isUnderstoodAs o ls = do
  modifyObject o (#understandAs %~ S.union (makeUnderstandAsSets ls))

makeUnderstandAsSets :: [Text] -> Set (Set Text)
makeUnderstandAsSets = S.fromList . map (S.fromList . words)