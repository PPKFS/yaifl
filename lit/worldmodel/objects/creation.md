# Creating Objects

```haskell file=src/Yaifl/Objects/Create.hs
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Create where
import Solitude
import Yaifl.Objects.Object
import Yaifl.Objects.Specifics
import Yaifl.Objects.Dynamic
import Yaifl.Common
import Yaifl.Properties.Property
import Yaifl.Properties.Enclosing
import Yaifl.Logger
import Yaifl.Objects.Query
import Yaifl.Objects.ObjectData
import Cleff.State
import Yaifl.Objects.Move

data ObjectCreation :: Effect where
  GenerateEntity :: Bool -> ObjectCreation m Entity

makeEffect ''ObjectCreation

-- | Create a new object and assign it an entity ID, but do **not** add it to any
-- stores. See also 'addObject' for a version that adds it to a store.
makeObject :: 
  Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics (WMObjSpecifics wm) -- ^ Object details.
  -> d
  -> Maybe (ObjectUpdate wm d) -- ^ 'Nothing' for a static object, 'Just f' for a dynamic object.
  -> Eff es (Entity, AbstractObject wm d)
makeObject n d ty isT specifics details upd = do
  e <- generateEntity isT
  t <- getGlobalTime
  let obj = Object n d e ty t specifics details
  return (e, maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd)

addObject :: 
  Subset es1 es
  => WMHasProperty wm Enclosing
  => (ObjectLike wm (AbstractObject wm d))
  => (AbstractObject wm d -> Eff es1 ())
  -> Text
  -> Text
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics (WMObjSpecifics wm)
  -> d
  -> Maybe (ObjectUpdate wm d)
  -> Eff es Entity
-- hilariously the pointfree version of this is
-- addObject = (. makeObject) . (.) . (.) . (.) . (.) . (.) . uncurry
addObject updWorld n d ty isT specifics details updateFunc = do
  (e, obj) <- makeObject n d ty isT specifics details updateFunc
  debug $ bformat ("Made a new " %! stext %! " called " %! stext %! " with ID " %! int)
    (if isThing obj then "thing" else "room") n e
  inject $ updWorld obj
  asThing <- getThingMaybe e
  lastRoom <- use previousRoom
  case asThing of
    Nothing -> previousRoom .= e
    Just t -> move t lastRoom >> pass -- move it if we're still 
  return e

-- | Create a new 'Thing' and add it to the relevant stores.
addThing ::
  WMHasProperty wm Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics (WMObjSpecifics wm))
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdate wm ThingData) -- ^ Static/Dynamic.
  -> Eff es Entity
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
  --WMHasProperty wm Enclosing
  Text -- ^ Name.
  -> Text -- ^ Description.
  -> Eff '[State ThingData] r -- ^ Build your own thing monad!
  -> Eff es Entity
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ execState stateUpdate blankThingData) Nothing

-- | Create a new 'Room' and add it to the relevant stores.
addRoom :: 
  WMHasProperty wm Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics (WMObjSpecifics wm))
  -> Maybe (RoomData wm) -- ^
  -> Maybe (ObjectUpdate wm (RoomData wm))  -- ^
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
  WMHasProperty wm Enclosing
  => Text
  -> Text
  -> Eff '[State (RoomData wm)] v
  -> m Entity
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just (execState rd blankRoomData)) Nothing

addBaseObjects ::
   WMHasProperty wm Enclosing
  => m ()
addBaseObjects = do
  addRoom' "The Void" "If you're seeing this, you did something wrong." pass
  addThing' "player" "It's you, looking handsome as always" (
    thingDescribed .= Undescribed)
  firstRoom .= Nothing
```
