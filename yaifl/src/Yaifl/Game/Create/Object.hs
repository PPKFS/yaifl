{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Yaifl.Game.Create.Object
  ( addRoomInternal
  , addThing
  , addObject
  , addRoom
  , addRoom'
  , addRegion
  , done
  , AddObjects
  ) where

import Yaifl.Prelude
import Breadcrumbs

import Yaifl.Core.Metadata
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Game.Move ( move )
import Yaifl.Core.ObjectLike
import Yaifl.Model.Query
import Yaifl.Core.Kinds.Room ( RoomData, blankRoomData, Room (..), tagRoomEntity, isVoid )
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Enclosing ( Enclosing )
import Yaifl.Model.WorldModel

import qualified Data.Set as S
import Yaifl.Model.Kinds.Region (RegionEntity, Region (..))
import Data.Char (isUpper)
import qualified Data.Text as T
import Yaifl.Core.Tag (tagObject)
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Text.Say

done = defaults

-- | Type synonym for adding new objects.
type AddObjects wm es = (
  ObjectUpdate wm :> es
  , Display (WMText wm)
  , IsString (WMText wm)
  , State Metadata :> es
  , Pointed (WMObjSpecifics wm)
  , Breadcrumbs :> es, ObjectUpdate wm :> es, ObjectLookup wm :> es
  , SayableValue (WMText wm) wm
  )

makeObject ::
  Display (WMText wm)
  => Pointed s
  => ObjectUpdate wm :> es
  => State Metadata :> es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
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
  forall wm s d es.
  Pointed s
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => (Object wm d s -> Eff es ())
  -> WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
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
                (void . move @(EnclosingThing wm) @_ @_ t . tagObject @EnclosingEntity @EnclosingTag loc)
                (void . move t)
                encLoc
        )
        (\r -> #previousRoom .= tagRoomEntity r) obj'
    pure obj

addThingInternal ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm
  -> WMText wm -- ^ Description.
  -> ObjectKind -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (ThingData wm)
  -> Maybe EnclosingEntity
  -> Eff es ThingEntity
addThingInternal name ia desc objtype specifics details mbLoc = do
  t <- Thing <$> addObject (setThing . Thing) name desc objtype
        True specifics (fromMaybe (blankThingData ia) details) mbLoc
  pure (tagThingEntity t)

addThing ::
  forall wm es.
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "specifics" :? WMObjSpecifics wm
  -> "modify" :? Eff '[State (Thing wm)] () -- ^ Build your own thing monad!
  -> "location" :? EnclosingEntity
  -> "type" :? ObjectKind
  -> "thingData" :? ThingData wm
  -> "portable" :? ThingPortable
  -> Eff es ThingEntity
addThing n
  (argDef #initialAppearance "" -> ia)
  (argDef #description "" -> d)
  (argF #specifics -> s)
  (argF #modify -> stateUpdate)
  (argF #location -> loc)
  (argDef #type (ObjectKind "thing") -> ki)
  (argDef #thingData (blankThingData ia) -> td)
  (argF #portable -> p) = do
    let td' = td & (#portable %~ maybe id const p)
    t <- addThingInternal n ia d ki s (Just td') loc
    whenJust stateUpdate $ \su -> failHorriblyIfMissing $ modifyThing t (`runLocalState` su)
    pure t

runLocalState :: a1 -> Eff '[State a1] a2 -> a1
runLocalState bl upd = snd $ runPureEff $ runStateLocal bl upd

addRoomInternal ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> ObjectKind -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (RoomData wm) -- ^
  -> Eff es RoomEntity
addRoomInternal name desc objtype specifics details = do
  e <- Room <$> addObject (setRoom . Room) name desc objtype False specifics (fromMaybe blankRoomData details) Nothing
  md <- get
  when (isVoid $ md ^. #firstRoom) (#firstRoom .= tagRoomEntity e)
  return (tagRoomEntity e)

addRoom' ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> Eff '[State (RoomData wm)] v
  -> Eff es RoomEntity
addRoom' n d rd = addRoomInternal n d (ObjectKind "room")
  Nothing (Just $ snd $ runPureEff $ runStateLocal blankRoomData rd)

addRoom ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "description" :? WMText wm -- ^ Description.
  -> Eff es RoomEntity
addRoom n (argDef #description "" -> d) = addRoom' n d pass

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