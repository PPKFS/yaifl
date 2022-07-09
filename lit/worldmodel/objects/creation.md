# Creating Objects

```haskell file=src/Yaifl/Core/Objects/Create.hs
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.Create
  ( -- * Effect
  ObjectCreation(..)
  , generateEntity
  , addAbstractThing
  , addAbstractRoom
  , addThing
  , addRoom
  , addThing'
  , addRoom'
  , addBaseObjects
  ) where

import Cleff.State ( State, get, runState )
import Solitude
import Yaifl.Core.Common
import Yaifl.Core.Logger ( debug, Log )
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Objects.Move ( move )
import Yaifl.Core.Objects.Object ( ObjType(ObjType), Object(Object) )
import Yaifl.Core.Objects.ObjectData
import Yaifl.Core.Objects.Query ( ObjectQuery, ObjectUpdate )
import Yaifl.Core.Objects.Specifics ( ObjectSpecifics(NoSpecifics) )
import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Property ( WMHasProperty )
import Yaifl.Core.Objects.Query (ObjectLookup)

<<creation-effect>>
<<make-object>>
<<add-objects>>
<<base-objects>>
```

```haskell id=creation-effect
data ObjectCreation wm :: Effect where
  GenerateEntity :: Bool -> ObjectCreation wm m Entity 
  AddAbstractThing :: AbstractThing wm -> ObjectCreation wm m ()
  AddAbstractRoom :: AbstractRoom wm -> ObjectCreation wm m ()

makeEffect ''ObjectCreation

type AddObjects wm es = '[ObjectCreation wm, State (Metadata wm), Log, ObjectUpdate wm, ObjectLookup wm] :>> es
```

```haskell id=make-object
makeObject :: 
  ObjectCreation wm :> es
  => State (Metadata wm) :> es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics (WMObjSpecifics wm) -- ^ Object details.
  -> d
  -> Maybe (ObjectUpdateFunc wm d) -- ^ 'Nothing' for a static object, 'Just f' for a dynamic object.
  -> Eff es (Entity, AbstractObject wm d)
makeObject n d ty isT specifics details upd = do
  e <- generateEntity isT
  t <- getGlobalTime
  let obj = Object n d e ty t specifics details
  return (e, maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd)

addObject :: 
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => (AbstractObject wm d -> Eff es ())
  -> Text
  -> Text
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics (WMObjSpecifics wm)
  -> d
  -> Maybe (ObjectUpdateFunc wm d)
  -> Eff es Entity
addObject updWorld n d ty isT specifics details updateFunc = do
  (e, obj) <- makeObject n d ty isT specifics details updateFunc
  debug $ bformat ("Made a new " %! stext %! " called " %! stext %! " with ID " %! int)
    (if isThing obj then "thing" else "room") n e
  updWorld obj
  lastRoom <- use previousRoom
  if
     isThing e 
  then
    previousRoom .= e
  else
    move e lastRoom >> pass -- move it if we're still 
  return e
```

-- | A version of 'addRoom' that uses a state monad to provide imperative-like
-- descriptions of the internals of the object. Compare
-- @
-- addThing n d o (Just $ (ThingData default default default .. mod1)) ...
-- @ with @
-- addThing' n d o (someLensField .= 5)
-- @

```haskell id=add-objects
addThing ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics (WMObjSpecifics wm))
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdateFunc wm ThingData) -- ^ Static/Dynamic.
  -> Eff es Entity
addThing name desc objtype specifics details = addObject addAbstractThing name desc objtype
  True (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blankThingData details)

addThing' :: 
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> Eff '[State ThingData] r -- ^ Build your own thing monad!
  -> Eff es Entity
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ snd $ runPure $ runState blankThingData stateUpdate) Nothing

addRoom :: 
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics (WMObjSpecifics wm))
  -> Maybe (RoomData wm) -- ^
  -> Maybe (ObjectUpdateFunc wm (RoomData wm))  -- ^
  -> Eff es Entity
addRoom name desc objtype specifics details upd = do
  e <- addObject addAbstractRoom name desc objtype False
        (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blankRoomData details) upd
  md <- get
  when (isVoid $ md ^. firstRoom) (firstRoom .= e)
  return e

isVoid :: Entity -> Bool
isVoid = (defaultVoidID ==)

addRoom' :: 
 WMHasProperty wm Enclosing
  => AddObjects wm es
  => Text
  -> Text
  -> Eff '[State (RoomData wm)] v
  -> Eff es Entity
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just $ snd $ runPure $ runState blankRoomData rd) Nothing
```

```haskell id=base-objects
addBaseObjects ::
  WMHasProperty wm Enclosing
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  addRoom' "The Void" "If you're seeing this, you did something wrong." pass
  addThing' "player" "It's you, looking handsome as always" (thingDescribed .= Undescribed)
  firstRoom .= defaultVoidID
```
