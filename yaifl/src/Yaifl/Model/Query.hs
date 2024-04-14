module Yaifl.Model.Query
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
  , getContainingHierarchy
  , getAllObjectsInRoom
  , IncludeDoors(..)
  , isUnderstoodAs
  , IncludeScenery(..)
  , areInRegion
  , isInRegion
  , isSubregionOf
  , getPropertyOrThrow
  , defaultPropertyGetter
  , defaultPropertySetter
  , modifyProperty
  , getEnclosingMaybe
  , getEnclosing
  , setEnclosing
  , HasProperty(..)
  , EnclosingObject(..)
  ) where

import Solitude
import Breadcrumbs

import Data.List.NonEmpty as NE (cons)
import Data.Text.Display
import Effectful.Optics ( use )

import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Effects
import Yaifl.Model.Entity
import Yaifl.Model.ObjectLike
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Room
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Tag

import qualified Data.EnumSet as ES
import qualified Data.Set as S
import Yaifl.Model.Kinds.Region
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Effectful.Error.Static (Error, throwError)

getThingMaybe ::
  ObjectLookup wm :> es
  => WithMetadata es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Thing wm))
getThingMaybe e = withoutMissingObjects (preview _Thing <$> getObject (getID e)) (const $ pure Nothing)

getRoomMaybe ::
  ObjectLookup wm :> es
  => WithMetadata es
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

modifyRegion ::
  NoMissingObjects wm es
  => RegionEntity
  -> (Region wm -> Region wm)
  -> Eff es ()
modifyRegion o u = do
  r <- lookupRegion o
  whenRight_ r $ \r' -> setRegion (u r')

modifyObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> Eff es ()
modifyObject e s = do
  o <- getObject e
  asThingOrRoom
    (`modifyThing` anyModifyToThing s)
    (`modifyRoom` anyModifyToRoom s) o

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

getContainingHierarchy ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es (NonEmpty EnclosingEntity)
getContainingHierarchy tLike = do
  let getHierarchy obj = do
        let enc = obj ^. #objectData % #containedBy
        o' <- getObject enc
        asThingOrRoom
          (\v -> do
            rs <- getHierarchy v
            pure (enc `NE.cons` rs)
          )
          (\r -> pure $ coerceTag (tagRoom r) :| []) o'
  getHierarchy tLike

isUnderstoodAs ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> [Text]
  -> Eff es ()
isUnderstoodAs o ls = do
  modifyObject o (#understandAs %~ S.union (makeUnderstandAsSets ls))

makeUnderstandAsSets :: [Text] -> Set (Set Text)
makeUnderstandAsSets = S.fromList . map (S.fromList . words)

areInRegion ::
  NoMissingObjects wm es
  => Foldable f
  => f RoomEntity
  -> RegionEntity
  -> Eff es ()
areInRegion f r = mapM_ (`isInRegion` r) f

isInRegion ::
  NoMissingObjects wm es
  => RoomEntity
  -> RegionEntity
  -> Eff es ()
isInRegion r reg = do
  modifyRegion reg (#rooms %~ S.insert r)

isSubregionOf ::
  NoMissingObjects wm es
  => RegionEntity
  -> RegionEntity
  -> Eff es ()
isSubregionOf subReg reg = do
  modifyRegion reg (#subRegions %~ S.insert subReg)

roomsInRegion ::
  NoMissingObjects wm es
  => Region wm
  -> Eff es (S.Set RoomEntity)
roomsInRegion r = do
  subRegs <- rights <$> mapM lookupRegion (S.toList $ subRegions r)
  rs <- mapM roomsInRegion subRegs
  pure $ S.unions $ rooms r : rs


getPropertyOrThrow ::
  HasID i
  => Error MissingObject :> es
  => Text
  -> i
  -> Maybe v
  -> Eff es v
getPropertyOrThrow t o = maybe (throwError $ MissingObject ("Could not find " <> t) (getID o)) pure

defaultPropertySetter ::
  NoMissingObjects wm es
  => WMWithProperty wm v
  => CanBeAny wm o
  => o
  -> v
  -> Eff es ()
defaultPropertySetter e v = modifyObject (toAny e) (#specifics % propertyAT .~ v)

defaultPropertyGetter ::
  forall wm o v.
  WMWithProperty wm v
  => CanBeAny wm o
  => o
  -> Maybe v
defaultPropertyGetter o = preview (#specifics % propertyAT) (toAny o)

modifyProperty ::
  CanBeAny wm o
  => (AnyObject wm -> Maybe p)
  -> (AnyObject wm -> p -> Eff es ())
  -> o
  -> (p -> p)
  -> Eff es ()
modifyProperty g s o f = do
  let e = g (toAny o)
  when (isNothing e) (do
    --logVerbose "Trying to modify a property of an object which does not exist"
    pass)
  whenJust e (s (toAny o) . f)

getEnclosingMaybe ::
  forall wm.
  WMWithProperty wm Enclosing
  => AnyObject wm
  -> Maybe Enclosing
getEnclosingMaybe e = asThingOrRoom
  (const $ defaultPropertyGetter e)
  (Just . view (#objectData % #enclosing)) e

setEnclosing ::
  forall wm es o.
  NoMissingObjects wm es
  => WMWithProperty wm Enclosing
  => CanBeAny wm o
  => o
  -> Enclosing
  -> Eff es ()
setEnclosing e v = asThingOrRoom
  (`defaultPropertySetter` v)
  (\o -> modifyRoom o (#objectData % #enclosing .~ v)) (toAny @wm e)

getEnclosing ::
  WMWithProperty wm Enclosing
  => EnclosingEntity
  -> AnyObject wm
  -> Enclosing
getEnclosing _ = fromMaybe (error "property witness was violated") . getEnclosingMaybe

-- | A lens that is guaranteed by witnesses
class HasProperty w o v where
  propertyL :: w -> Lens' o v

instance MayHaveProperty o v => HasProperty w o v where
  propertyL _ = lens (fromMaybe (error "property witness was violated") . preview propertyAT) (flip (set propertyAT))

class EnclosingObject o where
  enclosingL :: Lens' o Enclosing

instance EnclosingObject (Room wm) where
  enclosingL = #objectData % #enclosing

instance WMWithProperty wm Enclosing => EnclosingObject (EnclosingEntity, Thing wm) where
  enclosingL = lens (\(e, o) -> getEnclosing e (toAny o)) (\(e, o) enc -> (e, o & (#specifics % propertyAT .~ enc)))