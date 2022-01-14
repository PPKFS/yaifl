{-|
Module      : Yaifl.Common
Description : Mostly defining types to be used everywhere and some helper functions.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}
module Yaifl.Common
  (-- * Smart constructors and default settings
  -- * Object querying
  isThing
  , isRoom
  , HasID(..)
  , CanBeAny(..)

  -- * Lenses

  , objectL
  , containedBy

  -- * World lookups and modifications
  , getGlobalTime
  , tickGlobalTime
  , setTitle
  , newEntityID

  , reifyObject

  , isType
  , module Yaifl.Types
  )
where

import Yaifl.Prelude
import Yaifl.Messages
import Yaifl.Types

objectL :: Lens' (AbstractObject s d) (Object s d)
objectL = lens
  (\case
    StaticObject o -> o
    DynamicObject (TimestampedObject o _ _) -> o)
  (\o n -> case o of
    StaticObject _ -> StaticObject n
    DynamicObject ts -> DynamicObject $ set tsCachedObject n ts
  )

containedBy :: forall s. Lens' (Thing s) Entity
containedBy = coercedTo @(Object s ThingData) % objData % thingContainedBy

isThing
  :: (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom
  :: (HasID a)
  => a
  -> Bool
isRoom = not . isThing

reifyObject
  :: StoreLens' s d
  -> AbstractObject s d
  -> World s
  -> (Object s d, World s)
reifyObject _ (StaticObject v) w = (v, w)
reifyObject l (DynamicObject t) w = if _tsCacheStamp t == getGlobalTime w
                    then (co, w)
                    else runState (do
                      -- update the object
                      updatedObj <- gets $ updateObject (runUpdateFunction t) co
                      -- update the world
                      l % at (getID co) ?= DynamicObject
                        (updateCachedObject t updatedObj)
                      return updatedObj) w
                    where co = _tsCachedObject t
                          runUpdateFunction = _tsUpdateFunc

class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id

instance HasID (Object s d) where
  getID = _objID

instance HasID (AbstractObject s d) where
  getID (StaticObject o) = getID o
  getID (DynamicObject ts) = getID ts

instance HasID (TimestampedObject s d) where
  getID (TimestampedObject o _ _) = getID o

-- | Obtain the current timestamp. This is a function in case I want to change the
-- implementation in the future.
getGlobalTime
  :: World o
  -> Timestamp
getGlobalTime = _globalTime

tickGlobalTime
  :: State (World o) ()
tickGlobalTime = do
  globalTime %= (+1)
  logVerbose "Dong."

-- | Update the game title.
setTitle
  :: Text -- ^ New title.
  -> State (World o) ()
setTitle = (title .=)

-- | Generate a new entity ID.
newEntityID
  :: Bool
  -> World o
  -> (Entity, World o)
newEntityID True = entityCounter % _1 <<+~ 1
newEntityID False = entityCounter % _2 <<-~ 1

-- | Calculate whether one object type is a subclass of another
isType
  :: Object s d
  -> ObjType
  -> World s
  -> Bool
isType _ _ _ = False

class CanBeAny o d where
  toAny :: o -> d
  fromAny :: d -> Maybe o

instance CanBeAny (Object s RoomData) (AnyObject s) where
  toAny = fmap Right
  fromAny = traverse rightToMaybe

instance CanBeAny (Object s ThingData) (AnyObject s) where
  toAny = fmap Left
  fromAny = traverse leftToMaybe

instance CanBeAny (AbstractObject s RoomData) (AnyAbstractObject s) where
  toAny (StaticObject s) = StaticObject $ toAny s
  toAny (DynamicObject (TimestampedObject tsobj tsts (ObjectUpdate tsf))) =
    DynamicObject $ TimestampedObject
    (toAny tsobj) tsts (ObjectUpdate $ \a w -> maybe a (\v -> Right <$> tsf v w) (fromAny a))

  fromAny ((StaticObject s)) = fmap StaticObject (fromAny s)
  fromAny ((DynamicObject
    (TimestampedObject tsobj tsts (ObjectUpdate tsf)))) = case fromAny tsobj of
    Nothing -> Nothing
    Just s -> Just $ DynamicObject
      (TimestampedObject s tsts (ObjectUpdate $ \v w -> fromMaybe v (fromAny $ tsf (toAny v) w) ))

instance CanBeAny (AbstractObject s ThingData) (AnyAbstractObject s) where
  toAny (StaticObject s) = StaticObject $ toAny s
  toAny (DynamicObject (TimestampedObject tsobj tsts (ObjectUpdate tsf))) =
    DynamicObject $ TimestampedObject
    (toAny tsobj) tsts (ObjectUpdate $ \a w -> maybe a (\v -> Left <$> tsf v w) (fromAny a))

  fromAny ((StaticObject s)) = fmap StaticObject (fromAny s)
  fromAny ((DynamicObject
    (TimestampedObject tsobj tsts (ObjectUpdate tsf)))) = case fromAny tsobj of
    Nothing -> Nothing
    Just s -> Just $ DynamicObject
      (TimestampedObject s tsts (ObjectUpdate $ \v w -> fromMaybe v (fromAny $ tsf (toAny v) w) ))