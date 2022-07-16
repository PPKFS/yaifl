-- ~\~ language=Haskell filename=src/Yaifl/Core/Objects/Create.hs
-- ~\~ begin <<lit/worldmodel/objects/creation.md|src/Yaifl/Core/Objects/Create.hs>>[0] project://lit/worldmodel/objects/creation.md:4
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.Create
  ( -- * Effect
  ObjectCreation(..)
  , AddObjects
  , generateEntity
  , addAbstractThing
  , addAbstractRoom
  , addThing
  , addRoom
  , addThing'
  , addObject
  , addRoom'
  , addBaseObjects
  , reifyRoom
  , reifyThing
  ) where

import Cleff.State ( State, get, runState )

import Yaifl.Core.Common
import Yaifl.Core.Logger ( debug, Log )
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Objects.Move ( move )
import Yaifl.Core.Objects.Object ( ObjType(ObjType), Object(Object), Room, Thing, objData )
import Yaifl.Core.Objects.ObjectData
import Yaifl.Core.Objects.Query ( ObjectUpdate, ObjectLookup, getThing, failHorriblyIfMissing )
import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Property ( WMHasProperty )
import Text.Interpolation.Nyan

-- ~\~ begin <<lit/worldmodel/objects/creation.md|creation-effect>>[0] project://lit/worldmodel/objects/creation.md:40
data ObjectCreation wm :: Effect where
  GenerateEntity :: Bool -> ObjectCreation wm m Entity
  AddAbstractThing :: AbstractThing wm -> ObjectCreation wm m ()
  AddAbstractRoom :: AbstractRoom wm -> ObjectCreation wm m ()

makeEffect ''ObjectCreation

type AddObjects wm es = (ObjectCreation wm :> es, State (Metadata wm) :> es, Log :> es, ObjectUpdate wm :> es, ObjectLookup wm :> es)


-- | Turn an `AbstractObject` into a regular `Object` and update the cache if needed.
reifyObject ::
  State (Metadata wm) :> es
  => (AbstractObject wm d -> Eff es ())
  -> AbstractObject wm d
  -> Eff es (Object wm d)
reifyObject _ (StaticObject v) = return v
reifyObject setFunc (DynamicObject ts) = do
  let co = _tsCachedObject ts
  now <- getGlobalTime
  if
    _tsCacheStamp ts == now
  then
    return co
  else
    do
      -- update the object
      updatedObj <- runObjectUpdate (_tsUpdateFunc ts) co
      t <- getGlobalTime
      setFunc (DynamicObject $ TimestampedObject updatedObj t (_tsUpdateFunc ts))
      return updatedObj

reifyRoom ::
  State (Metadata wm) :> es
  => (ObjectCreation wm :> es)
  => AbstractRoom wm
  -> Eff es (Room wm)
reifyRoom = reifyObject addAbstractRoom

reifyThing ::
  State (Metadata wm) :> es
  => (ObjectCreation wm :> es)
  => AbstractThing wm
  -> Eff es (Thing wm)
reifyThing = reifyObject addAbstractThing
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/creation.md|make-object>>[0] project://lit/worldmodel/objects/creation.md:51
makeObject ::
  ObjectCreation wm :> es
  => State (Metadata wm) :> es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType
  -> Bool
  -> Maybe (WMObjSpecifics wm) -- ^ Object details.
  -> d
  -> Maybe (ObjectUpdateFunc wm d) -- ^ 'Nothing' for a static object, 'Just f' for a dynamic object.
  -> Eff es (Entity, AbstractObject wm d)
makeObject n d ty isT specifics details upd = do
  e <- generateEntity isT
  t <- getGlobalTime
  let obj = Object n d e ty t specifics details
  return (e, maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd)

addObject ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => (AbstractObject wm d -> Eff es (Object wm d))
  -> (AbstractObject wm d -> Eff es ())
  -> Text
  -> Text
  -> ObjType
  -> Bool
  -> Maybe (WMObjSpecifics wm)
  -> d
  -> Maybe (ObjectUpdateFunc wm d)
  -> Eff es (Object wm d)
addObject rf updWorld n d ty isT specifics details updateFunc = do
  (e, obj) <- makeObject n d ty isT specifics details updateFunc
  let (n' :: Text) = if isThing obj then "thing" else "room"
  debug [int|t| Made a new #{n'} called #{n} with ID #{e} |]
  updWorld obj
  lastRoom <- use previousRoom
  if
     isRoom e
  then
    previousRoom .= e
  else
    failHorriblyIfMissing $ do
      t <- getThing e
      when (t ^. objData % thingContainedBy == defaultVoidID)
        (move t lastRoom >> pass)
  rf obj
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/creation.md|add-objects>>[0] project://lit/worldmodel/objects/creation.md:104
addThing ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdateFunc wm ThingData) -- ^ Static/Dynamic.
  -> Eff es (Thing wm)
addThing name desc objtype specifics details = addObject reifyThing addAbstractThing name desc objtype
  True specifics (fromMaybe blankThingData details)

addThing' ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> Eff '[State ThingData] r -- ^ Build your own thing monad!
  -> Eff es (Thing wm)
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ snd $ runPure $ runState blankThingData stateUpdate) Nothing

addRoom ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (RoomData wm) -- ^
  -> Maybe (ObjectUpdateFunc wm (RoomData wm))  -- ^
  -> Eff es (Room wm)
addRoom name desc objtype specifics details upd = do
  e <- addObject reifyRoom addAbstractRoom name desc objtype False specifics (fromMaybe blankRoomData details) upd
  md <- get
  when (isVoid $ md ^. firstRoom) (firstRoom .= getID e)
  return e

isVoid :: Entity -> Bool
isVoid = (defaultVoidID ==)

addRoom' ::
 WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text
  -> Text
  -> Eff '[State (RoomData wm)] v
  -> Eff es (Room wm)
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just $ snd $ runPure $ runState blankRoomData rd) Nothing
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/creation.md|base-objects>>[0] project://lit/worldmodel/objects/creation.md:159
addBaseObjects ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  addRoom' "The Void" "If you're seeing this, you did something wrong." pass
  addThing' "player" "It's you, looking handsome as always" (thingDescribed .= Undescribed)
  firstRoom .= defaultVoidID
-- ~\~ end
-- ~\~ end
