{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Objects.Query
  ( -- * Types
  ObjectLike(..)
  , MissingObject(..)
  , ObjectQuery
  , ObjectLookup(..)
  , ObjectUpdate(..)
  , ObjectTraverse(..)
  -- * Missing Objects
  , withoutMissingObjects
  , failHorriblyIfMissing
  , handleMissingObject
  , NoMissingObjects
  , NoMissingRead
  -- * Get
  , lookupThing
  , lookupRoom
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
  -- * Set
  , setThing
  , setRoom
  , traverseRooms
  , traverseThings

  , refreshRoom
  , refreshThing

  , getCurrentPlayer
  ) where

import Solitude

import Effectful.Error.Static ( runError, throwError, Error )
import Effectful.Optics ( use )
import Effectful.TH ( makeEffect )

import Yaifl.Model.Entity ( HasID(..), Entity)
import Yaifl.Metadata
import Yaifl.Model.Object
import Yaifl.Model.Objects.ThingData
import Yaifl.Model.WorldModel (WorldModel, WMSayable)
import Breadcrumbs
import Data.Text.Display

data MissingObject = MissingObject
  { expected :: Text
  , entity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

withoutMissingObjects ::
  HasCallStack
  => (HasCallStack => Eff (Error MissingObject ': es) a) -- ^ the block
  -> (HasCallStack => MissingObject -> Eff es a)  -- ^ the handler
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

data ObjectLookup (wm :: WorldModel) :: Effect where
  LookupThing :: HasID o => o -> ObjectLookup wm m (Either Text (Thing wm))
  LookupRoom :: HasID o => o -> ObjectLookup wm m (Either Text (Room wm))

data ObjectUpdate (wm :: WorldModel) :: Effect where
  SetRoom :: Room wm -> ObjectUpdate wm m ()
  SetThing :: Thing wm -> ObjectUpdate wm m ()

data ObjectTraverse (wm :: WorldModel) :: Effect where
  TraverseThings :: (Thing wm -> m (Maybe (Thing wm))) -> ObjectTraverse wm m ()
  TraverseRooms :: (Room wm -> m (Maybe (Room wm))) -> ObjectTraverse wm m ()

makeEffect ''ObjectLookup
makeEffect ''ObjectUpdate
makeEffect ''ObjectTraverse

type ObjectQuery wm es = (ObjectLookup wm :> es, ObjectUpdate wm :> es)
type NoMissingObjects wm es = (WithMetadata es, Error MissingObject :> es, ObjectQuery wm es, Display (WMSayable wm))
type NoMissingRead wm es = (Error MissingObject :> es, ObjectLookup wm :> es, WithMetadata es)
type ObjectRead wm es = (ObjectLookup wm :> es, WithMetadata es)

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

getObject ::
  NoMissingRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (AnyObject wm)
getObject e =
  if isThing (getID e)
    then review _Thing <$> getThing e
    else review _Room <$> getRoom e

getThingMaybe ::
  ObjectRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Thing wm))
getThingMaybe e = do
  if isThing (getID e)
    then withoutMissingObjects (getThing e <&> Just) (const (return Nothing))
  else return Nothing

getRoomMaybe ::
  ObjectRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Room wm))
getRoomMaybe e = do
  if isRoom (getID e)
    then withoutMissingObjects (getRoom e <&> Just) (const (return Nothing))
  else return Nothing

asThingOrRoom ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> a)
  -> (Room wm -> a)
  -> Eff es a
asThingOrRoom o tf rf = do
  if isThing (getID o)
    then tf <$> getThing o
  else rf <$> getRoom o

asThingOrRoomM ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> Eff es a)
  -> (Room wm -> Eff es a)
  -> Eff es a
asThingOrRoomM o tf rf = do
  if isThing (getID o)
    then getThing o >>= tf
  else getRoom o >>= rf

modifyObjectFrom ::
  (o -> Eff es (Object wm any))
  -> (Object wm any -> Eff es ())
  -> o
  -> (Object wm any -> Object wm any)
  -> Eff es ()
modifyObjectFrom g s o u = do
  obj <- g o
  s (u obj)
  pass

modifyThing ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> Thing wm)
  -> Eff es ()
modifyThing = modifyObjectFrom getThing setThing

modifyRoom ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Room wm -> Room wm)
  -> Eff es ()
modifyRoom = modifyObjectFrom getRoom setRoom

modifyObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> Eff es ()
modifyObject e s = do
  if isThing (getID e)
  then modifyThing e (anyModifyToThing s)
  else modifyRoom e (anyModifyToRoom s)

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
  => ObjectLike wm o
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
    r' <- getRoom $ objectId r
    when (r' /= r) $ noteError (const ()) $ "Refreshed room with ID" <> show (objectId r) <> " and found an outdated object"
    return r)
  (pure r)

refreshThing ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es (Thing wm)
refreshThing r = ifM (traceGuard Medium)
  (do
    r' <- getThing $ objectId r
    when (r' /= r) $ noteError (const ()) $ "Refreshed thing with ID" <> show (objectId r) <> " and found an outdated object"
    return r)
  (pure r)

getCurrentPlayer ::
  Breadcrumbs :> es
  => ObjectLookup wm :> es
  => State Metadata :> es
  => Eff es (Thing wm)
getCurrentPlayer = failHorriblyIfMissing $ use #currentPlayer >>= getThing
