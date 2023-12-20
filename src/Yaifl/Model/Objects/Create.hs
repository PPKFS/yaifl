{-# LANGUAGE ViewPatterns #-}
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
import Yaifl.Model.Properties.Has ( WMWithProperty )
import Yaifl.Model.WorldModel ( WMObjSpecifics, WMSayable )
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.ObjectLike
import Named

makeObject ::
  Pointed s
  => ObjectCreation wm :> es
  => State Metadata :> es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectType
  -> Bool
  -> Maybe s -- ^ Object details.
  -> d
  -> Eff es (Entity, Object wm d s)
makeObject n d ty isT specifics details = do
  e <- generateEntity isT
  t <- getGlobalTime
  return (e, Object n Nothing Nothing [] SingularNamed Improper d e ty t t (fromMaybe identityElement specifics) details)

addObject ::
  Pointed s
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => (Object wm d s -> Eff es ())
  -> WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectType
  -> Bool
  -> Maybe s
  -> d
  -> Maybe EnclosingEntity
  -> Eff es (Object wm d s)
addObject updWorld n d ty isT specifics details mbLocation =
  withSpan' ("new " <> if isT then "thing" else "room") (display n) $ do
    (e, obj) <- makeObject n d ty isT specifics details
    addAnnotation "object created"
    updWorld obj
    addAnnotation "object added to world"
    lastRoomE <- use #previousRoom
    tickGlobalTime
    failHorriblyIfMissing $ do
      obj' <- getObject e
      lastRoom <- getRoom lastRoomE
      asThingOrRoom
        (\t -> do
          case mbLocation of
            Nothing -> move t lastRoom >> pass
            Just loc -> do
              encLoc <- getObject loc
              asThingOrRoom
                (void . move t . (loc,))
                (void . move t)
                encLoc
        )
        (\r -> #previousRoom .= tagRoom r) obj'
    pure obj

addThingInternal ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm
  -> WMSayable wm -- ^ Description.
  -> ObjectType -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (ThingData wm)
  -> Maybe EnclosingEntity
  -> Eff es (Thing wm)
addThingInternal name ia desc objtype specifics details mbLoc =
  Thing <$> addObject (addThingToWorld . Thing) name desc objtype
    True specifics (fromMaybe (blankThingData ia) details) mbLoc

addThing' ::
  forall wm es r.
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> "initialAppearance" :? WMSayable wm
  -> "description" :? WMSayable wm -- ^ Description.
  -> "specifics" :? WMObjSpecifics wm
  -> "build" :! Eff '[State (ThingData wm)] r -- ^ Build your own thing monad!
  -> "location" :? EnclosingEntity
  -> Eff es (Thing wm)
addThing' n
  (argDef #initialAppearance "" -> ia)
  (argDef #description "" -> d)
  (argF #specifics -> s)
  (arg #build -> stateUpdate)
  (argF #location -> loc) =
    addThingInternal n ia d (ObjectType "thing") s (runLocalState (blankThingData ia) stateUpdate) loc

runLocalState :: a1 -> Eff '[State a1] a2 -> Maybe a1
runLocalState bl upd = Just $ snd $ runPureEff $ runStateLocal bl upd

addThing ::
  forall wm es.
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> "initialAppearance" :? WMSayable wm
  -> "description" :? WMSayable wm -- ^ Description.
  -> "specifics" :? WMObjSpecifics wm
  -> Eff es (Thing wm)
addThing n (argDef #initialAppearance "" -> ia) (argDef #description "" -> d) (argDef #specifics identityElement -> s) =
  addThing' @wm n
    ! #initialAppearance ia
    ! #description d
    ! #specifics s
    ! #build pass
    ! defaults

addRoomInternal ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectType -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (RoomData wm) -- ^
  -> Eff es (Room wm)
addRoomInternal name desc objtype specifics details = do
  e <- Room <$> addObject (addRoomToWorld . Room) name desc objtype False specifics (fromMaybe blankRoomData details) Nothing
  md <- get
  when (isVoid $ md ^. #firstRoom) (#firstRoom .= tagRoom e)
  return e

addRoom' ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff '[State (RoomData wm)] v
  -> Eff es (Room wm)
addRoom' n d rd = addRoomInternal n d (ObjectType "room")
  Nothing (Just $ snd $ runPureEff $ runStateLocal blankRoomData rd)

addRoom ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff es (Room wm)
addRoom n d = addRoom' n d pass

addBaseObjects ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  v <- addRoom "The Void" "If you're seeing this, you did something wrong."
  addThing' "player" ! #description "It's you, looking handsome as always" ! #build (#described .= Undescribed) ! defaults
  #firstRoom .= tagRoom v