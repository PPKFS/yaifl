module Yaifl.Core.Query.Enclosing
  ( getAllObjectsInRoom
  , getAllObjectsInEnclosing
  , getContainingHierarchy
  , IncludeDoors(..)
  , IncludeScenery(..)
  , RecurseAllObjects(..)
  , getEnclosingMaybe
  , setEnclosing
  , IsEnclosingObject(..)
  , IsEnclosing(..)
  , getEnclosingObject
  , enclosingContains
  , getCommonAncestor
  , getLocation
  ) where

import Yaifl.Prelude

import Data.List.NonEmpty as NE (cons)

import Yaifl.Core.Kinds.Object
import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.ObjectLike
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Tag

import qualified Data.EnumSet as ES
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Model.WorldModel
import Yaifl.Model.Query

data IncludeScenery = IncludeScenery | ExcludeScenery
data IncludeDoors = IncludeDoors | ExcludeDoors
data RecurseAllObjects = Recurse | DontRecurse

getLocation ::
  NoMissingObjects wm es
  => ThingLike wm o
  => o
  -> Eff es (Room wm)
getLocation t = do
  t' <- getThing t
  o <- getObject (t' ^. #objectData % #containedBy)
  asThingOrRoom getLocation return o

getAllObjectsInRoom ::
  RoomLike wm o
  => WMWithProperty wm Enclosing
  => NoMissingObjects wm es
  => IncludeScenery
  -> IncludeDoors
  -> RecurseAllObjects
  -> o
  -> Eff es [Thing wm]
getAllObjectsInRoom incScenery incDoors recurse r = do
  r' <- getRoom r
  getAllObjectsInEnclosing incScenery incDoors recurse (coerceTag $ tagRoomEntity r')

getAllObjectsInEnclosing ::
  NoMissingObjects wm es
  => WMWithProperty wm Enclosing
  => IncludeScenery
  -> IncludeDoors
  -> RecurseAllObjects
  -> EnclosingEntity
  -> Eff es [Thing wm]
getAllObjectsInEnclosing incScenery incDoors recurse r = do
  e <- getEnclosingObject r
  let e' = getEnclosing e
  let allItemIDs = ES.toList $ e' ^. #contents
  things <- mapM getThing allItemIDs
  -- recurse downwards
  recursedThings <- mconcat <$> mapM (\t -> do
    let mbE = getEnclosingMaybe (toAny t)
    case mbE of
      Just enc' -> getAllObjectsInEnclosing incScenery incDoors recurse (tagEntity enc' t)
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
          (\r -> pure $ coerceTag (tagRoomEntity r) :| []) o'
  getHierarchy tLike


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
