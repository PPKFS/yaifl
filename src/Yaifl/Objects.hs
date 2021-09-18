
module Yaifl.Objects (
) where

import Relude
import Yaifl.Common
import Control.Lens
import Control.Lens.Internal.CTypes (Int)
import qualified Data.IntMap.Strict as IM
import Yaifl (AbstractObject)

-- moving should only be possible with:
-- things
-- and rooms or things with containable properties
class IsContainer t where

type Enclosing = Int

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

blah :: Lens' (World t r c) (Store (AbstractObject t r c o)) -> AbstractObject t r c o -> World t r c -> (Object o, World t r c)
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