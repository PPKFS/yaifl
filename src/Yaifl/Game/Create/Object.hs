{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Yaifl.Game.Create.Object
  ( addRoomInternal
  , addThing
  , addObject
  , addRoom
  , addRoom'
  , addRegion
  , addBaseObjects
  , done
  ) where

import Solitude
import Breadcrumbs

import Data.Text.Display
import Effectful.Optics ( (.=), use )
import Named

import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Effects
import Yaifl.Model.Entity
import Yaifl.Game.Move ( move )
import Yaifl.Model.ObjectLike
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Room ( RoomData, blankRoomData, tagRoom, Room (..) )
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Enclosing ( Enclosing )
import Yaifl.Model.HasProperty ( WMWithProperty )
import Yaifl.Model.WorldModel

import qualified Data.Set as S
import Yaifl.Model.Kinds.Region (RegionEntity, Region (..))
import Data.Char (isUpper)
import qualified Data.Text as T

done = defaults

makeObject ::
  Display (WMSayable wm)
  => Pointed s
  => ObjectUpdate wm :> es
  => State Metadata :> es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectKind
  -> Bool
  -> Maybe s -- ^ Object details.
  -> d
  -> Eff es (Entity, Object wm d s)
makeObject n d ty isT specifics details = do
  e <- generateEntity isT
  t <- getGlobalTime
  let shownName = display n
  return (e, Object n Nothing PubliclyNamed Nothing S.empty SingularNamed
    (if not (T.null shownName) && isUpper (T.head shownName) then Proper else Improper) d e ty t t (fromMaybe identityElement specifics) details)

addObject ::
  Pointed s
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => (Object wm d s -> Eff es ())
  -> WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectKind
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
  -> ObjectKind -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (ThingData wm)
  -> Maybe EnclosingEntity
  -> Eff es ThingEntity
addThingInternal name ia desc objtype specifics details mbLoc = do
  t <- Thing <$> addObject (setThing . Thing) name desc objtype
        True specifics (fromMaybe (blankThingData ia) details) mbLoc
  pure (tagThing t)

addThing ::
  forall wm es.
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> "initialAppearance" :? WMSayable wm
  -> "description" :? WMSayable wm -- ^ Description.
  -> "specifics" :? WMObjSpecifics wm
  -> "modify" :? Eff '[State (Thing wm)] () -- ^ Build your own thing monad!
  -> "location" :? EnclosingEntity
  -> "type" :? ObjectKind
  -> "thingData" :? ThingData wm
  -> Eff es ThingEntity
addThing n
  (argDef #initialAppearance "" -> ia)
  (argDef #description "" -> d)
  (argF #specifics -> s)
  (argF #modify -> stateUpdate)
  (argF #location -> loc)
  (argDef #type (ObjectKind "thing") -> ki)
  (argDef #thingData (blankThingData ia) -> td) = do
    t <- addThingInternal n ia d ki s (Just td) loc
    whenJust stateUpdate $ \su -> failHorriblyIfMissing $ modifyThing t (`runLocalState` su)
    pure t

runLocalState :: a1 -> Eff '[State a1] a2 -> a1
runLocalState bl upd = snd $ runPureEff $ runStateLocal bl upd

addRoomInternal ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> ObjectKind -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (RoomData wm) -- ^
  -> Eff es RoomEntity
addRoomInternal name desc objtype specifics details = do
  e <- Room <$> addObject (setRoom . Room) name desc objtype False specifics (fromMaybe blankRoomData details) Nothing
  md <- get
  when (isVoid $ md ^. #firstRoom) (#firstRoom .= tagRoom e)
  return (tagRoom e)

addRoom' ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> WMSayable wm -- ^ Description.
  -> Eff '[State (RoomData wm)] v
  -> Eff es RoomEntity
addRoom' n d rd = addRoomInternal n d (ObjectKind "room")
  Nothing (Just $ snd $ runPureEff $ runStateLocal blankRoomData rd)

addRoom ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMSayable wm -- ^ Name.
  -> "description" :? WMSayable wm -- ^ Description.
  -> Eff es RoomEntity
addRoom n (argDef #description "" -> d) = addRoom' n d pass

addBaseObjects ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  v <- addRoom "The Void" ! #description "If you're seeing this, you did something wrong." ! done
  addThing "player" ! #description "It's you, looking handsome as always" ! #modify (#objectData % #described .= Undescribed) ! done
  #firstRoom .= v

addRegion ::
  Pointed (WMRegionData wm)
  => ObjectUpdate wm :> es
  => Text
  -> Eff es RegionEntity
addRegion n = do
  rId <- generateEntity False
  let r = Region (unsafeTagEntity rId) n PubliclyNamed S.empty Nothing S.empty identityElement
  setRegion r
  pure (unsafeTagEntity rId)