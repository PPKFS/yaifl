module Yaifl.Model.Objects.Create
  ( addThingInternal
  , addRoomInternal
  , addThing'
  , addThing
  , addObject
  , addRoom
  , addRoom'
  , addBaseObjects
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display
import Effectful.Optics ( (.=), use )

import Yaifl.Model.Entity
import Yaifl.Metadata
import Yaifl.Model.Object
import Yaifl.Model.Objects.Move ( move )
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.RoomData ( RoomData, blankRoomData )
import Yaifl.Model.Objects.ThingData
import Yaifl.Model.Properties.Enclosing ( Enclosing )
import Yaifl.Model.Properties.Has ( WMHasProperty )
import Yaifl.Model.WorldModel ( WMObjSpecifics, WMSayable )
import Yaifl.Model.Objects.Effects

makeObject ::
  ObjectCreation wm :> es
  => State Metadata :> es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectType
  -> Bool
  -> Maybe (WMObjSpecifics wm) -- ^ Object details.
  -> d
  -> Eff es (Entity, Object wm d)
makeObject n d ty isT specifics details = do
  e <- generateEntity isT
  t <- getGlobalTime
  return (e, Object n Nothing Nothing SingularNamed Improper d e ty t t specifics details)

addObject ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => (Object wm d -> Eff es ())
  -> WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectType
  -> Bool
  -> Maybe (WMObjSpecifics wm)
  -> d
  -> Eff es (Object wm d)
addObject updWorld n d ty isT specifics details =
  withSpan' ("new " <> if isT then "thing" else "room") (display n) $ do
    (e, obj) <- makeObject n d ty isT specifics details
    addAnnotation "object created"
    updWorld obj
    addAnnotation "object added to world"
    lastRoom <- use #previousRoom
    tickGlobalTime
    if
      isRoom e
    then
      #previousRoom .= e
    else
      failHorriblyIfMissing $ do
        t <- getThing e
        withoutSpan $ when (t ^. #objectData % #containedBy == voidID)
          (move t lastRoom >> pass)
    pure obj

addThingInternal ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectType -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Eff es (Thing wm)
addThingInternal name desc objtype specifics details =
  addObject addThingToWorld name desc objtype
    True specifics (fromMaybe blankThingData details)

addThing' ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff '[State ThingData] r -- ^ Build your own thing monad!
  -> Eff es (Thing wm)
addThing' n d stateUpdate = addThingInternal n d (ObjectType "thing")
    Nothing (Just $ snd $ runPureEff $ runStateLocal blankThingData stateUpdate)

addThing ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff es (Thing wm)
addThing n d = addThing' n d pass

addRoomInternal ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectType -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (RoomData wm) -- ^
  -> Eff es (Room wm)
addRoomInternal name desc objtype specifics details = do
  e <- addObject addRoomToWorld name desc objtype False specifics (fromMaybe blankRoomData details)
  md <- get
  when (isVoid $ md ^. #firstRoom) (#firstRoom .= getID e)
  return e

addRoom' ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff '[State (RoomData wm)] v
  -> Eff es (Room wm)
addRoom' n d rd = addRoomInternal n d (ObjectType "room")
  Nothing (Just $ snd $ runPureEff $ runStateLocal blankRoomData rd)

addRoom ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff es (Room wm)
addRoom n d = addRoom' n d pass

addBaseObjects ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  addRoom "The Void" "If you're seeing this, you did something wrong."
  addThing' "player" "It's you, looking handsome as always" (#described .= Undescribed)
  #firstRoom .= voidID