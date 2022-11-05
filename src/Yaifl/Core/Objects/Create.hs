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

import Solitude

import Effectful.Optics ( (.=), use )
import Effectful.TH ( makeEffect )

import Yaifl.Core.AdaptiveText ( AdaptiveText )
import Yaifl.Core.Entity ( HasID(getID), Entity, voidID )
import Yaifl.Core.Logger ( debug, Log )
import Yaifl.Core.Metadata
import Yaifl.Core.Object
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Objects.Move ( move )
import Yaifl.Core.Objects.Query ( ObjectUpdate, ObjectLookup, getThing, failHorriblyIfMissing )
import Yaifl.Core.Objects.RoomData ( RoomData, blankRoomData )
import Yaifl.Core.Objects.ThingData
import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Has ( WMHasProperty )
import Yaifl.Core.WorldModel ( WMObjSpecifics )

data ObjectCreation wm :: Effect where
  GenerateEntity :: Bool -> ObjectCreation wm m Entity
  AddAbstractThing :: AbstractObject wm ThingData -> ObjectCreation wm m ()
  AddAbstractRoom :: AbstractObject wm (RoomData wm) -> ObjectCreation wm m ()

makeEffect ''ObjectCreation

type AddObjects wm es = (ObjectCreation wm :> es, State Metadata :> es, Log :> es, ObjectUpdate wm :> es, ObjectLookup wm :> es)

-- | Turn an `AbstractObject` into a regular `Object` and update the cache if needed.
reifyObject ::
  State Metadata :> es
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
  State Metadata :> es
  => (ObjectCreation wm :> es)
  => AbstractObject wm (RoomData wm)
  -> Eff es (Room wm)
reifyRoom = reifyObject addAbstractRoom

reifyThing ::
  State Metadata :> es
  => (ObjectCreation wm :> es)
  => AbstractObject wm ThingData
  -> Eff es (Thing wm)
reifyThing = reifyObject addAbstractThing

makeObject ::
  ObjectCreation wm :> es
  => State Metadata :> es
  => AdaptiveText (ObjectDomain wm) -- ^ Name.
  -> AdaptiveText (ObjectDomain wm) -- ^ Description.
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
  -> AdaptiveText (ObjectDomain wm) -- ^ Name.
  -> AdaptiveText (ObjectDomain wm) -- ^ Description.
  -> ObjType
  -> Bool
  -> Maybe (WMObjSpecifics wm)
  -> d
  -> Maybe (ObjectUpdateFunc wm d)
  -> Eff es (Object wm d)
addObject rf updWorld n d ty isT specifics details updateFunc = do
  (e, obj) <- makeObject n d ty isT specifics details updateFunc
  let (n' :: Text) = if isThing e then "thing" else "room"
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
      when (t ^. objData % thingContainedBy == voidID)
        (move t lastRoom >> pass)
  rf obj

addThing ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => AdaptiveText (ObjectDomain wm) -- ^ Name.
  -> AdaptiveText (ObjectDomain wm) -- ^ Description.
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
  => AdaptiveText (ObjectDomain wm) -- ^ Name.
  -> AdaptiveText (ObjectDomain wm) -- ^ Description.
  -> Eff '[State ThingData] r -- ^ Build your own thing monad!
  -> Eff es (Thing wm)
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ snd $ runPureEff $ runStateLocal blankThingData stateUpdate) Nothing

addRoom ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => AdaptiveText (ObjectDomain wm) -- ^ Name.
  -> AdaptiveText (ObjectDomain wm) -- ^ Description.
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
isVoid = (voidID ==)

addRoom' ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => AdaptiveText (ObjectDomain wm) -- ^ Name.
  -> AdaptiveText (ObjectDomain wm) -- ^ Description.
  -> Eff '[State (RoomData wm)] v
  -> Eff es (Room wm)
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just $ snd $ runPureEff $ runStateLocal blankRoomData rd) Nothing

addBaseObjects ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  addRoom' "The Void" "If you're seeing this, you did something wrong." pass
  addThing' "player" "It's you, looking handsome as always" (thingDescribed .= Undescribed)
  firstRoom .= voidID