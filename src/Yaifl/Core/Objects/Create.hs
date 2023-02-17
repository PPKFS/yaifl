{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.Create
  ( -- * Effect
  ObjectCreation(..)
  , AddObjects
  , generateEntity
  , addThing
  , addRoom
  , addThingInternal
  , addRoomInternal
  , addThing'
  , addObject
  , addRoom'
  , addBaseObjects
  ) where

import Solitude

import Effectful.Optics ( (.=), use )
import Effectful.TH ( makeEffect )

import Yaifl.Core.Entity ( HasID(getID), Entity, voidID )
import Yaifl.Core.Metadata
import Yaifl.Core.Object
import Yaifl.Core.Objects.Move ( move )
import Yaifl.Core.Objects.Query ( ObjectUpdate, ObjectLookup, getThing, failHorriblyIfMissing )
import Yaifl.Core.Objects.RoomData ( RoomData, blankRoomData )
import Yaifl.Core.Objects.ThingData
import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Has ( WMHasProperty )
import Yaifl.Core.WorldModel ( WMObjSpecifics, WMSayable )
import Breadcrumbs
import Data.Text.Display

data ObjectCreation wm :: Effect where
  GenerateEntity :: Bool -> ObjectCreation wm m Entity
  AddThing :: Object wm ThingData -> ObjectCreation wm m ()
  AddRoom :: Object wm (RoomData wm) -> ObjectCreation wm m ()

makeEffect ''ObjectCreation

type AddObjects wm es = (
  ObjectCreation wm :> es
  , Display (WMSayable wm)
  , IsString (WMSayable wm)
  , State Metadata :> es
  , Breadcrumbs :> es, ObjectUpdate wm :> es, ObjectLookup wm :> es)

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
  let obj = Object n d e ty t specifics details
  return (e, obj)

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
addThingInternal name desc objtype specifics details = addObject addThing name desc objtype
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
  e <- addObject addRoom name desc objtype False specifics (fromMaybe blankRoomData details)
  md <- get
  when (isVoid $ md ^. #firstRoom) (#firstRoom .= getID e)
  return e

isVoid :: Entity -> Bool
isVoid = (voidID ==)

addRoom' ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff '[State (RoomData wm)] v
  -> Eff es (Room wm)
addRoom' n d rd = addRoomInternal n d (ObjectType "room")
  Nothing (Just $ snd $ runPureEff $ runStateLocal blankRoomData rd)

addBaseObjects ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  addRoom' "The Void" "If you're seeing this, you did something wrong." pass
  addThing' "player" "It's you, looking handsome as always" (#described .= Undescribed)
  #firstRoom .= voidID