{-|
Module      : Yaifl.Objects.Create
Description : Making objects.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Objects.Create where
import Yaifl.World
import Solitude
import Yaifl.Objects.Object
import Yaifl.ObjectSpecifics
import Yaifl.Objects.Dynamic
import Yaifl.Common
import Yaifl.Properties.Property
import Yaifl.Properties.Enclosing
import Yaifl.Logger
import Yaifl.Objects.Query
import Yaifl.Objects.ObjectData
import Yaifl.Objects.Move

-- | Create a new object and assign it an entity ID, but do **not** add it to any
-- stores. See also 'addObject' for a version that adds it to a store.
makeObject :: 
  MonadWorld wm m
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics (WMObjSpecifics wm) -- ^ Object details.
  -> d
  -> Maybe (ObjectUpdate wm d) -- ^ 'Nothing' for a static object, 'Just f' for
                                  -- a dynamic object.
  -> m (Entity, AbstractObject wm d)
makeObject n d ty isT specifics details upd = do
  e <- state $ newEntityID isT
  t <- gets getGlobalTime
  let obj = Object n d e ty t specifics details
  return (e, maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd)

addObject :: 
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => (ObjectLike wm (AbstractObject wm d))
  => (forall m1. MonadWorld wm m1 => AbstractObject wm d -> m1 ())
  -> Text
  -> Text
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics (WMObjSpecifics wm)
  -> d
  -> Maybe (ObjectUpdate wm d)
  -> m Entity
-- hilariously the pointfree version of this is
-- addObject = (. makeObject) . (.) . (.) . (.) . (.) . (.) . uncurry
addObject updWorld n d ty isT specifics details updateFunc = do
  obj <- makeObject n d ty isT specifics details updateFunc
  debug $ bformat ("Made a new " %! stext %! " called " %! stext %! " with ID " %! int)
    (if isThing (snd obj) then "thing" else "room") n (getID $ snd obj)
  updWorld (snd obj)
  let e = snd obj
  asThing <- getThingMaybe e
  lastRoom <- use previousRoom
  case asThing of
    Nothing -> previousRoom .= fst obj
    Just t -> move t lastRoom >> pass -- move it if we're still 
  return (fst obj)

-- | Create a new 'Thing' and add it to the relevant stores.
addThing ::
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics (WMObjSpecifics wm))
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdate wm ThingData) -- ^ Static/Dynamic.
  -> m Entity
addThing name desc objtype specifics details = addObject setAbstractThing name desc objtype
  True (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blankThingData details)

-- | A version of 'addThing' that uses a state monad to provide imperative-like
-- descriptions of the internals of the object. Compare
-- @
-- addThing n d o (Just $ (ThingData default default default .. mod1)) ...
-- @ with @
-- addThing' n d o (someLensField .= 5)
-- @
addThing' :: 
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> State ThingData r -- ^ Build your own thing monad!
  -> m Entity
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ execState stateUpdate blankThingData) Nothing

-- | Create a new 'Room' and add it to the relevant stores.
addRoom :: 
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics (WMObjSpecifics wm))
  -> Maybe RoomData -- ^
  -> Maybe (ObjectUpdate wm RoomData)  -- ^
  -> m Entity
addRoom name desc objtype specifics details upd = do
  e <- addObject setAbstractRoom name desc objtype False
        (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blankRoomData details) upd
  w <- get
  when (isNothing $ w ^. firstRoom) (firstRoom ?= e)
  return e

-- | A version of 'addRoom' that uses a state monad to provide imperative-like
-- descriptions of the internals of the object. Compare
-- @
-- addThing n d o (Just $ (ThingData default default default .. mod1)) ...
-- @ with @
-- addThing' n d o (someLensField .= 5)
-- @
addRoom' :: 
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => Text
  -> Text
  -> State RoomData v
  -> m Entity
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just (execState rd blankRoomData)) Nothing

addBaseObjects ::
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => m ()
addBaseObjects = do
  addRoom' "The Void" "If you're seeing this, you did something wrong." pass
  addThing' "player" "It's you, looking handsome as always" (
    thingDescribed .= Undescribed)
  firstRoom .= Nothing