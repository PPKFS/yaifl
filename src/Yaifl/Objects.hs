
{-|
Module      : Yaifl.Objects
Description : Creating, modifying, querying objects.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}
module Yaifl.Objects
( -- * Adding objects
  addRoom
, addRoom'
, addThing
, addThing'
, addObject

, move
) where

import Relude
import Yaifl.Common
import Control.Lens

type Enclosing = Int

-- | Get the ID out of an abstract object. Since the ID **should** be constant, using
-- the cached version of a dynamic object isn't a problem.
getAbstractObjectID
  :: AbstractObject t r c o
  -> Entity
getAbstractObjectID (StaticObject o') = _objID o'
getAbstractObjectID (DynamicObject t) = (_objID . _tsCachedObject) t

-- TODO: can I make this into a functor?
updateInternal
  :: Lens' (World u r c) (Store (AbstractObject u r c o))
  -> AbstractObject u r c o
  -> World u r c
  -> World u r c
updateInternal storeLens obj = storeLens . at (getAbstractObjectID obj) ?~ obj

-- | Update a 'room'.
updateRoom
  :: AbstractRoom t r c
  -> World t r c
  -> World t r c
updateRoom = updateInternal rooms

-- | Update a 'thing'.
updateThing
  :: AbstractThing t r c
  -> World t r c
  -> World t r c
updateThing = updateInternal things

-- | Create a new object and assign it an entity ID, but do **not** add it to any
-- stores. See also 'addObject' for a version that adds it to a store.
makeObject
  :: Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType
  -> o -- ^ Object details.
  -> Maybe (ObjectUpdate u r c o) -- ^ 'Nothing' for a static object, 'Just f' for
                                  -- a dynamic object.
  -> World u r c
  -> ((Entity, AbstractObject u r c o), World u r c)
makeObject n d ty specifics upd = runState $ do
  e <- state newEntityID
  t <- gets getGlobalTime
  let obj = Object n d e ty t specifics
  return (e, maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd)

addObject
  :: (AbstractObject u r c o -> World u r c -> World u r c)
  -> Text
  -> Text
  -> ObjType
  -> o
  -> Maybe (ObjectUpdate u r c o)
  -> State (World u r c) Entity
-- hilariously the pointfree version of this is
-- addObject = (. makeObject) . (.) . (.) . (.) . (.) . (.) . uncurry
addObject updWorld n d ty specifics updateFunc = do
  obj <- state $ makeObject n d ty specifics updateFunc
  modify $ updWorld (snd obj)
  return (fst obj)

-- | Create a new 'Thing' and add it to the relevant stores.
addThing
  :: IsSubtype t ThingProperties
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (ThingData t) -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdate t r c (ThingData t)) -- ^ Static/Dynamic.
  -> State (World t r c) Entity
addThing name desc objtype details = addObject updateThing
  name desc objtype (fromMaybe blankThingData details)

-- | A version of 'addThing' that uses a state monad to provide imperative-like
-- descriptions of the internals of the object. Compare
-- @
-- addThing n d o (Just $ (ThingData default default default .. mod1)) ...
-- @ with @
-- addThing' n d o (someLensField .= 5)
-- @
addThing'
  :: IsSubtype t ThingProperties
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> State (ThingData t) v -- ^ Build your own thing monad!
  -> State (World t r c) Entity
addThing' n d stateUpdate = addThing n d (ObjType "thing")
  (Just (execState stateUpdate blankThingData)) Nothing

addRoom
  :: IsSubtype r RoomProperties
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (RoomData r) -- ^
  -> Maybe (ObjectUpdate t r c (RoomData r))  -- ^ Optional details.
  -> State (World t r c) Entity
addRoom name desc objtype details = addObject updateRoom
  name desc objtype (fromMaybe blankRoomData details)

-- | See 'addThing`'.
addRoom'
  :: IsSubtype r RoomProperties
  => Text
  -> Text
  -> State (RoomData r) v
  -> State (World t r c) Entity
addRoom' n d rd = addRoom n d (ObjType "room")
  (Just (execState rd blankRoomData)) Nothing

move
  :: Entity
  -> Entity
  -> World t r c
  -> (Maybe Bool, World t r c)
move = error "not impl"
{-
getThing
  :: Entity
  -> World t r c
  -> (Maybe (Thing t), World t r c)
getThing = getObjectFrom things

getObjectFrom
  :: Lens' (World t r c) (Store (AbstractObject t r c o))
  -> Entity
  -> World t r c
  -> (Maybe (Object o), World t r c)
getObjectFrom f e = runState $ do
    o <- use $ f . at (unID e)
    w <- get
    (o' :: Maybe (Object o)) <- maybe (return Nothing) (\o' -> do
        obj <- state $ blah f o'
        return $ Just obj) o
    return Nothing

blah :: Lens' (World t r c) (Store (AbstractObject t r c o)) -- ^
  -> AbstractObject t r c o -- ^
  -> World t r c -- ^
  -> (Object o, World t r c)
blah _ (StaticObject v) w = (v, w)
blah l (DynamicObject t) w = if _cacheStamp t == getGlobalTime w
                    then (co, w)
                    else runState (do
                        updatedObj <- gets (flip (_updateFunc t) co)
                        let tsUpdated = DynamicObject $ updateCachedObject t updatedObj
                        l . at (unID $ _objID co) ?= tsUpdated
                        return updatedObj) w
                    where co = _cachedObject t

updateCachedObject :: TimestampedObject t r c o -> Object o -> TimestampedObject t r c o
updateCachedObject = error "not implemented"


enclosingPrism :: Prism' (Object a) (Object Enclosing)
enclosingPrism = undefined

getEnclosing :: Entity -> World t r c -> (Maybe Enclosing, World t r c)
getEnclosing = error "not implemented"

move :: Entity -> Entity -> World t r c -> (Maybe Text, World t r c)
move eObj eLoc = runState $ do
    obj <- state $ getThing eObj
    loc <- state $ getEnclosing eLoc

    -- obtain both objects
    -- obtain the containing component
    -- obtain the current location of the thing
    -- change the location of the thing to the container
    -- remove the thing from its previous container
    -- update the timestamp
    return Nothing


    {-
    objToMove <- getThing obj
    mloc <- getComponent @Enclosing le -- use $ gameWorld . (store @w @Enclosing) . at le
    locName <- getComponent @(Object w) le
    doIfExists2
        objToMove
        mloc
        (showMaybeObjDebug objToMove <> " has no physical component, so cannot be moved.")
        (showMaybeObjDebug locName <> " has no enclosing component, so cannot move objects into it.")
        ( \o _ -> do
            --todo: recalc the location?
            -- todo: doesn't this mean the location is actually
            -- a derived property?
            let vl = o ^. thingPhysical . enclosedBy
            vlo <- getComponent @(Object w) vl
            logDebug $ "Moving " <> showObjDebug o <> " from " <> showMaybeObjDebug vlo <> " to " <> showMaybeObjDebug locName
            adjustComponent @(Physical w) obj (enclosedBy .~ le)
            adjustComponent @Enclosing vl (encloses %~ DS.delete obj)
            adjustComponent @Enclosing le (encloses %~ DS.insert obj)
            return True
        )
    -}
-}