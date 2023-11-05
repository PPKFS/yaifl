module Yaifl.Model.Objects.Query
  ( -- * Types
  ObjectLike(..)
  -- * Missing Objects
  , withoutMissingObjects
  , failHorriblyIfMissing
  , handleMissingObject

  -- * Get
  , getObject
  , getThingMaybe
  , getRoomMaybe
  , asThingOrRoom
  , asThingOrRoomM
  , getLocation
  -- * Modify
  , modifyObject
  , modifyThing
  , modifyRoom
  -- * SetmodifyObjectFrom
  , refreshRoom
  , refreshThing

  , getCurrentPlayer
  , isVoid
  ) where

import Solitude

import Breadcrumbs
import Effectful.Error.Static ( runError, Error )
import Effectful.Optics ( use )

import Yaifl.Metadata
import Yaifl.Model.Entity ( HasID(..), Entity, voidID )
import Yaifl.Model.Object
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.ObjectLike
import Yaifl.Model.Objects.ThingData
import Data.Text.Display

withoutMissingObjects ::
  HasCallStack
  => (HasCallStack => Eff (Error MissingObject ': es) a) -- ^ the block
  -> (HasCallStack => MissingObject -> Eff es a)  -- ^ the handler, which is basically always just "nothing"
  -> Eff es a
withoutMissingObjects f def = do
  r <- runError f
  case r of
    -- TODO: investigate what the callstack adds
    Left err' -> def (snd err')
    Right x -> return x

handleMissingObject ::
  WithMetadata es
  => Text
  -> a
  -> MissingObject
  -> Eff es a
handleMissingObject msg def (MissingObject t o) =
  noteError (const def) $ "When " <> show msg <> " the object with ID " <> show o <> " could not be found because " <> show t

failHorriblyIfMissing ::
  HasCallStack
  => Breadcrumbs :> es
  => (HasCallStack => Eff (Error MissingObject ': es) a)
  -> Eff es a
failHorriblyIfMissing f = withoutMissingObjects f (\(MissingObject t o) -> do
  let msg = "the object with ID " <> show o <> " could not be found because " <> show t <> ". We are failing horribly and erroring out because we can't recover."
  addAnnotation msg
  error msg)

getThingMaybe ::
  ObjectRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Thing wm))
getThingMaybe e = withoutMissingObjects (preview _Thing <$> getObject (getID e)) (const $ pure Nothing)

getRoomMaybe ::
  ObjectRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Room wm))
getRoomMaybe e = withoutMissingObjects (preview _Room <$> getObject (getID e)) (const $ pure Nothing)

asThingOrRoom ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> a)
  -> (Room wm -> a)
  -> Eff es a
asThingOrRoom o tf rf = do
  a <- getObject o
  case (preview _Thing a, preview _Room a) of
    (Just x, _) -> tf <$> getThing x
    (_, Just x) -> rf <$> getRoom x
    _ -> error "impossible"

asThingOrRoomM ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> Eff es a)
  -> (Room wm -> Eff es a)
  -> Eff es a
asThingOrRoomM o tf rf = join $ asThingOrRoom o tf rf

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
modifyThing o u = modifyObjectFrom (fmap (\(Thing a) -> a) . getThing) (setThing . Thing) o ((\(Thing a) -> a) . u . Thing)

modifyRoom ::
  NoMissingObjects wm es
  => RoomLike wm o
  => o
  -> (Room wm -> Room wm)
  -> Eff es ()
modifyRoom o u = modifyObjectFrom (fmap (\(Room a) -> a) . getRoom) (setRoom . Room) o ((\(Room a) -> a) . u . Room)

modifyObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> Eff es ()
modifyObject e s = asThingOrRoomM e
  (`modifyThing` anyModifyToThing s)
  (`modifyRoom` anyModifyToRoom s)

anyModifyToThing ::
  (AnyObject s -> AnyObject s)
  -> (Thing s -> Thing s)
anyModifyToThing f t = fromMaybe t (preview _Thing $ f (review _Thing t))

anyModifyToRoom ::
  (AnyObject s -> AnyObject s)
  -> (Room s -> Room s)
anyModifyToRoom f t = fromMaybe t (preview _Room $ f (review _Room t))

getLocation ::
  NoMissingObjects wm es
  => ThingLike wm o
  => o
  -> Eff es (Room wm)
getLocation t = do
  t' <- getThing t
  o <- getObject (t' ^. #objectData % #containedBy)
  join $ asThingOrRoom o getLocation return

refreshRoom ::
  NoMissingObjects wm es
  => Room wm
  -> Eff es (Room wm)
refreshRoom r = ifM (traceGuard Medium)
  (do
    r' <- getRoom (r ^. #objectId)
    when ((r' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed room with ID" <> show (display $ view #name r)  <> " and found an outdated object"
    return r')
  (pure r)

refreshThing ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es (Thing wm)
refreshThing r = ifM (traceGuard Medium)
  (do
    r' <- getThing (r ^. #objectId)
    when ((r' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed thing with ID" <> show (display $ view #name r) <> " and found an outdated object"
    return r')
  (pure r)

getCurrentPlayer ::
  Breadcrumbs :> es
  => ObjectLookup wm :> es
  => State Metadata :> es
  => Eff es (Thing wm)
getCurrentPlayer = failHorriblyIfMissing $ use #currentPlayer >>= getThing

isVoid ::
  Entity
  -> Bool
isVoid = (voidID ==)
