module Yaifl.Model.Objects.Query
  ( -- * Types
  ObjectLike(..)
  -- * Missing Objects
  , failHorriblyIfMissing

  -- * Get
  , getThingMaybe
  , getRoomMaybe
  , asThingOrRoom
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
  , tagObject

  , getAllObjectsInRoom
  , IncludeDoors(..)
  , IncludeScenery(..)
  ) where

import Solitude

import Breadcrumbs
import Effectful.Error.Static ( runError, Error )
import Effectful.Optics ( use )

import Yaifl.Metadata
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Object
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.ObjectLike
import Data.Text.Display
import qualified Data.EnumSet as ES
import Yaifl.Model.Objects.Tag

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
  (Thing wm -> a)
  -> (Room wm -> a)
  -> AnyObject wm
  -> a
asThingOrRoom tf rf a = case (preview _Thing a, preview _Room a) of
    (Just x, _) -> tf x
    (_, Just x) -> rf x
    _ -> error "impossible"

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
  => AnyObject wm
  -> (AnyObject wm -> AnyObject wm)
  -> Eff es ()
modifyObject e s = asThingOrRoom
  (`modifyThing` anyModifyToThing s)
  (`modifyRoom` anyModifyToRoom s) e

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
  asThingOrRoom getLocation return o

refreshRoom ::
  NoMissingObjects wm es
  => RoomLike wm o
  => o
  -> Eff es (Room wm)
refreshRoom tl = do
  r <- getRoom tl
  ifM (traceGuard Medium)
    (do
      r'' <- getRoom (tagRoom r)
      when ((r'' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed room with ID" <> show (display $ view #name r) <> " and found an outdated object"
      return r'')
    (pure r)

refreshThing ::
  NoMissingObjects wm es
  => ThingLike wm o
  => o
  -> Eff es (Thing wm)
refreshThing tl = do
  r <- getThing tl
  ifM (traceGuard Medium)
    (do
      r'' <- getThing (tagThing r)
      when ((r'' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed thing with ID" <> show (display $ view #name r) <> " and found an outdated object"
      return r'')
    (pure r)

getCurrentPlayer ::
  Breadcrumbs :> es
  => ObjectLookup wm :> es
  => State Metadata :> es
  => Eff es (Thing wm)
getCurrentPlayer = failHorriblyIfMissing $ use #currentPlayer >>= getThing

isVoid ::
  HasID a
  => a
  -> Bool
isVoid = (unTag voidID ==) . getID

tagObject ::
  AnyObject wm
  -> Either (TaggedEntity ThingTag) (TaggedEntity RoomTag)
tagObject a = case (preview _Thing a, preview _Room a) of
    (Just x, _) -> Left (tag x (a ^. #objectId))
    (_, Just x) -> Right (tag x (a ^. #objectId))
    _ -> error "impossible"

data IncludeScenery = IncludeScenery | ExcludeScenery
data IncludeDoors = IncludeDoors | ExcludeDoors

getAllObjectsInRoom ::
  RoomLike wm o
  => NoMissingObjects wm es
  => IncludeScenery
  -> IncludeDoors
  -> o
  -> Eff es [Thing wm]
getAllObjectsInRoom _incScenery _incDoors r = do
  room <- getRoom r
  let allItemIDs = ES.toList $ room ^. #objectData % #enclosing % #contents
  mapM getThing allItemIDs
