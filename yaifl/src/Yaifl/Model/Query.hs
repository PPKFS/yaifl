module Yaifl.Model.Query
  ( -- * Types
  ObjectLike(..)
  , Refreshable(..)
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
  , unwrapAny
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
  , setEnclosing
  , IsEnclosingObject(..)
  , IsEnclosing(..)
  , getEnclosingObject
  , getPlayer'
  , getDescribableContents
  , enclosingContains
  , getAllObjectsInEnclosing
  , getCommonAncestor

  , makeItScenery
  ) where

import Yaifl.Prelude
import Breadcrumbs

import Data.List.NonEmpty as NE (cons)

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
import Yaifl.Model.Store
import Data.Bitraversable

-- | All of the objects in the arguments are READ-ONLY. Whilst they can be swapped out, the
-- refresh function is called to replace and update the objects
class Refreshable wm av where
  refresh :: forall es. (NoMissingObjects wm es) => av -> Eff es av

instance {-# OVERLAPPING #-} (HasID o, Refreshable wm o) => Refreshable wm (TaggedObject o tagEntity) where
  refresh obj = tagObject obj <$> refresh (getTaggedObject obj)

instance (Refreshable wm a, Refreshable wm b) => Refreshable wm (a, b) where
  refresh (a, b) = refresh a >>= \a' -> refresh b >>= return . (a', )

instance Refreshable wm a => Refreshable wm (Store a) where
  refresh = mapM refresh

instance Refreshable wm a => Refreshable wm (Maybe a) where
  refresh t = case t of
    Nothing -> return t
    Just x -> Just <$> refresh x

instance (Refreshable wm b, Refreshable wm a) => Refreshable wm (Either a b) where
  refresh = bimapM refresh refresh

instance Refreshable wm Int where
  refresh = pure

instance Refreshable wm (Thing wm) where
  refresh t = getThing (tagThingEntity t)

instance Refreshable wm (AnyObject wm) where
  refresh t = getObject (getID t)

instance Refreshable wm () where
  refresh = const pass

instance Refreshable wm (Room wm) where
  refresh r = getRoom (tagRoom r)
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
      r'' <- getThing (tagThingEntity r)
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

unwrapAny ::
  AnyObject wm
  -> Either (TaggedEntity ThingTag) (TaggedEntity RoomTag)
unwrapAny a = case (preview _Thing a, preview _Room a) of
    (Just x, _) -> Left (tagEntity x (a ^. #objectId))
    (_, Just x) -> Right (tagEntity x (a ^. #objectId))
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

getAllObjectsInEnclosing ::
  NoMissingObjects wm es
  => WMWithProperty wm Enclosing
  => IncludeScenery
  -> IncludeDoors
  -> EnclosingEntity
  -> Eff es [Thing wm]
getAllObjectsInEnclosing incScenery incDoors r = do
  e <- getEnclosingObject r
  let e' = getEnclosing e
  let allItemIDs = ES.toList $ e' ^. #contents
  things <- mapM getThing allItemIDs
  -- recurse downwards
  recursedThings <- mconcat <$> mapM (\t -> do
    let mbE = getEnclosingMaybe (toAny t)
    case mbE of
      Just enc' -> getAllObjectsInEnclosing incScenery incDoors (tagEntity enc' t)
      Nothing -> return []) things
  enclosingItself <- getThingMaybe r
  return $ ordNub (maybeToList enclosingItself <> things <> recursedThings)

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

class IsEnclosingObject o where
  getEnclosing :: o -> Enclosing
  default getEnclosing :: CanBeAny wm o => WMWithProperty wm Enclosing => o -> Enclosing
  getEnclosing = fromMaybe (error "property witness was violated") . getEnclosingMaybe . toAny

class IsEnclosing o where
  getEnclosingEntity :: o -> EnclosingEntity
  default getEnclosingEntity :: HasID o => o -> EnclosingEntity
  getEnclosingEntity = unsafeTagEntity . getID

instance IsEnclosing RoomEntity where
  getEnclosingEntity = coerceTag

instance IsEnclosing EnclosingEntity where
  getEnclosingEntity = id

instance WMWithProperty wm Enclosing => IsEnclosingObject (TaggedObject (AnyObject wm) EnclosingTag) where
  getEnclosing = fromMaybe (error "property witness was violated") . getEnclosingMaybe . getTaggedObject

instance WMWithProperty wm Enclosing => IsEnclosingObject (TaggedObject (Thing wm) EnclosingTag) where
  getEnclosing = fromMaybe (error "property witness was violated") . getEnclosingMaybe . toAny . getTaggedObject

instance IsEnclosingObject (Room wm) where
  getEnclosing = view (#objectData % #enclosing)

getEnclosingObject :: (IsEnclosing o, HasCallStack, NoMissingRead wm es) => o -> Eff es (TaggedAnyEnclosing wm)
getEnclosingObject theObj = do
    let e = getEnclosingEntity theObj
    o <- getObject e
    let taggedObj = tagObject e o
    pure $ taggedObj

getPlayer' ::
  NoMissingObjects wm es
  => Eff es (Thing wm)
getPlayer' = use #currentPlayer >>= getThing

getDescribableContents ::
  NoMissingObjects wm es
  => Enclosing
  -> Eff es [Thing wm]
getDescribableContents e = do
  p <- getPlayer'
  catMaybes <$> mapM (\i -> do
    item <- getThing i
    if thingIsScenery item || p `objectEquals` i {- || todo: falsely unoccupied -}
    then pure Nothing
    else pure (Just item)
    ) (ES.toList $ view #contents e)

enclosingContains ::
  NoMissingObjects wm es
  => ThingLike wm o
  => EnclosingEntity
  -> o
  -> Eff es Bool
enclosingContains e o = do
  hier <- getContainingHierarchy =<< getThing o
  return $ e `elem` hier

getCommonAncestor ::
  NoMissingObjects wm es
  => ThingLike wm o1
  => ThingLike wm o2
  => o1
  -> o2
  -> Eff es EnclosingEntity
getCommonAncestor t1' t2' = do
  t1 <- getThing t1'
  t2 <- getThing t2'
  let actorHolder = thingContainedBy t1
      nounHolder = thingContainedBy t2
  if actorHolder == nounHolder
  then return actorHolder
  else
    do
      acHier <- getContainingHierarchy t1
      nounHier <- getContainingHierarchy t2
      -- we can cheat doing a proper lowest common ancestor. we can take one of the hierarchies
      -- (which one is irrelevant), and find the earliest possible match in the other list
      let commAncestor (l1h :| l1s) l2 = if l1h `elem` l2 then l1h else commAncestor
            (case l1s of
              [] -> error "no common ancestor"
              x:xs -> x :| xs) l2
      return $ commAncestor acHier nounHier

makeItScenery :: Eff '[State (Thing wm)] ()
makeItScenery = (#objectData % #isScenery .= True)
