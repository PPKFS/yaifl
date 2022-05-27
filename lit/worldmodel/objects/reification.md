# Reification

```haskell file=src/Yaifl/Objects/Dynamic.hs

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Dynamic 
  ( -- * Types
    TimestampedObject(..)
  , ObjectUpdate(..)
  , AbstractObject(..)
  , AbstractThing
  , AbstractRoom
  , AnyAbstractObject
    -- * Updates
  , updateCachedObject
  --, reifyObject
    -- * Lenses
  --, StoreLens'
  , tsCachedObject
  , tsCacheStamp
  , tsUpdateFunc
  --, _AbstractThing
  --, _AbstractRoom
  , objectL
  ) where

import Solitude
import Yaifl.Common
import Yaifl.Objects.Object
import Yaifl.Objects.ObjectData

-- | A 'TimestampedObject' is an object which has been cached at time '_tsCacheStamp'
-- and contains a function to update it given the state of the world. For instance,
-- this allows descriptions to be dynamic.
data TimestampedObject wm d = TimestampedObject
  { _tsCachedObject :: !(Object wm d)
  , _tsCacheStamp :: !Timestamp
  , _tsUpdateFunc :: ObjectUpdate wm d
  } deriving stock (Generic)

instance HasID (TimestampedObject wm d) where
  getID (TimestampedObject o _ _) = getID o

-- | Function to update an object. It is read-only on the world; i.e. it can only modify itself
newtype ObjectUpdate (wm :: WorldModel) d = ObjectUpdate
  { updateObject :: ()-- forall m. (MonadReader (World wm) m) => Object wm d -> m (Object wm d)
  } 

-- | An abstract object is either a static object (which does not need to update itself)
-- or a timestamped object. Whilst this is what is stored internally, you shouldn't
-- need to pass these around; instead reify the object with 'reifyObject'.
data AbstractObject wm d
  = DynamicObject (TimestampedObject wm d)
  | StaticObject (Object wm d)

instance HasID (AbstractObject wm d) where
  getID (StaticObject o) = getID o
  getID (DynamicObject ts) = getID ts

type AbstractThing wm = AbstractObject wm ThingData
type AbstractRoom wm = AbstractObject wm (RoomData wm)
type AnyAbstractObject wm = AbstractObject wm (Either ThingData (RoomData wm))

-- type StoreLens' wm d = (Lens' (World wm) (Store (AbstractObject wm d)))

makeLenses ''TimestampedObject
{-
instance ObjectLike wm (AbstractThing wm) where
  getThing = reifyObject things

instance ObjectLike wm (AbstractRoom wm) where
  getRoom = reifyObject rooms


_AbstractBase :: 
  Prism' (AnyObject wm) (Object wm d)
  -> Prism' (AnyAbstractObject wm) (AbstractObject wm d)
_AbstractBase p = prism' 
  (\case 
    -- static cases are just lifting through the constructor
    StaticObject s -> StaticObject $ review p s
    --a dynamic object can be lifted to `Any` by lifting through the cached object and wrapping the result of the update
    DynamicObject (TimestampedObject tsco tscs (ObjectUpdate tsf)) -> DynamicObject $ TimestampedObject
      (review p tsco) tscs (ObjectUpdate $ \a -> maybe (return a) (fmap (review p) . tsf ) (preview p a)))
  (\case
    StaticObject s -> fmap StaticObject (preview p s)
    -- what even is this?
    -- we fmap to get the Maybe outside of the constructor
    -- assuming that we indeed have a room, we want to apply the update function
    -- which only works on Any; this means we need to re-wrap (review) with the (fmap Left), run the update
    -- and then extract what (assuming we didn't screw up somewhere) will always be a Room again but in the case
    -- something changed, we just return the original version. TODO: maybe it should return Nothing to be lawful?
    DynamicObject (TimestampedObject tsobj tsts (ObjectUpdate tsf)) -> fmap 
      (\s -> DynamicObject (TimestampedObject s tsts (ObjectUpdate $ \v -> do
        r' <- tsf $ review p v
        return $ fromMaybe v $ preview p r') )) (tsobj ^? p))


-- | A prism for abstract rooms. 
_AbstractRoom :: Prism' (AnyAbstractObject wm) (AbstractRoom wm) 
_AbstractRoom = _AbstractBase _Room

_AbstractThing :: Prism' (AnyAbstractObject wm) (AbstractThing wm) 
_AbstractThing = _AbstractBase _Thing
-}
-- | A lens to reify (and therefore also set) an object, but without updating on get.
objectL ::
  Timestamp
  -> Lens' (AbstractObject wm d) (Object wm d)
objectL t = lens
  (\case
    StaticObject o -> o
    DynamicObject (TimestampedObject o _ _) -> o)
  (\o n -> case o of
    StaticObject _ -> StaticObject n
    DynamicObject ts -> DynamicObject (updateCachedObject ts n t)
  )

-- | Update a cached object at a specified time
updateCachedObject ::
  TimestampedObject wm d
  -> Object wm d
  -> Timestamp
  -> TimestampedObject wm d
updateCachedObject ts n t = ts & tsCachedObject .~ n
                               & tsCacheStamp .~ t
{-
-- | Turn an `AbstractObject` into a regular `Object` and update the cache if needed.
reifyObject ::
  MonadWorld wm m
  => StoreLens' wm d
  -> AbstractObject wm d
  -> m (Object wm d)
reifyObject _ (StaticObject v) = return v
reifyObject l (DynamicObject ts) = do
  let co = _tsCachedObject ts
  now <- getGlobalTime
  if
    _tsCacheStamp ts == now
  then
    return co
  else
    do
      -- update the object
      updatedObj <- updateObject (_tsUpdateFunc ts) co
      -- update the world
      t <- gets getGlobalTime
      l % at (getID co) ?= DynamicObject (updateCachedObject ts updatedObj t)
      return updatedObj
-}
```
