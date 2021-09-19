
module Yaifl.Objects (
    move,
    addThing,
    addThing',
    addRoom,
    addRoom',
) where

import Relude
import Yaifl.Common
import Control.Lens

type Enclosing = Int

updateInternal :: Lens' (World u r c) (Store (AbstractObject u r c o)) -> AbstractObject u r c o -> World u r c -> World u r c
updateInternal l o w = w & l . at (getID' o) ?~ o
  where
    getID' (StaticObject o') = _objID o'
    getID' (DynamicObject t) = (_objID . _cachedObject) t

updateRooms :: AbstractRoom t r c -> World t r c -> World t r c
updateRooms = updateInternal rooms

updateObjects :: AbstractThing t r c -> World t r c -> World t r c
updateObjects = updateInternal objects

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
  -> (AbstractObject u r c o, World u r c)
makeObject n d ty specifics upd = runState $ do
  e <- state newEntityID
  t <- gets getGlobalTime
  let obj = Object n d e ty t specifics
  return $ maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd

addInternal
  :: (AbstractObject u r c o -> World u r c -> World u r c) 
  -> Text
  -> Text
  -> ObjType
  -> o
  -> Maybe (ObjectUpdate u r c o)
  -> World u r c
  -> World u r c
-- hilariously the pointfree version of this is 
-- addInternal = (. makeObject) . (.) . (.) . (.) . (.) . (.) . uncurry
addInternal updWorld n d ty specifics updateFunc w = 
  uncurry updWorld (makeObject n d ty specifics updateFunc w)

addThing :: IsSubtype u ThingProperties => Text -> Text -> ObjType -> Maybe (ThingData u) -> Maybe (ObjectUpdate u r c (ThingData u)) -> World u r c -> World u r c
addThing n d ot spec = addInternal updateObjects n d ot (fromMaybe blankThingData spec)

addRoom :: IsSubtype r RoomProperties => Text -> Text -> ObjType -> Maybe (RoomData r) -> Maybe (ObjectUpdate u r c (RoomData r)) -> World u r c -> World u r c
addRoom n d ot spec = addInternal updateRooms n d ot (fromMaybe blankRoomData spec)

addThing' :: IsSubtype u ThingProperties => Text -> Text -> State (ThingData u) v -> World u r c -> World u r c
addThing' n d stateUpdate = addThing n d (ObjType "thing") (Just (execState stateUpdate blankThingData)) Nothing

addRoom' :: IsSubtype r RoomProperties => Text -> Text -> State (RoomData r) v -> World u r c -> World u r c
addRoom' n d rd = addRoom n d (ObjType "room") (Just (execState rd blankRoomData)) Nothing

getThing :: Entity -> World t r c -> (Maybe (Thing t), World t r c)
getThing = getObjectFrom objects

getObjectFrom :: Lens' (World t r c) (Store (AbstractObject t r c o)) -> Entity -> World t r c -> (Maybe (Object o), World t r c)
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