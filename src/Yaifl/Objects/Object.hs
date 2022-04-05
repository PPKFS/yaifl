{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Object where

import Solitude
import Yaifl.Common
import Yaifl.Objects.ObjectData
import Yaifl.Properties

-- | ObjTypes make a DAG that approximates inheritance; for instance, we may only care
-- that an object *is* a kind of food, but we don't necessarily know what the @a@ is
-- or looks like.
newtype ObjType = ObjType
  { unObjType :: Text 
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Ord)

-- | An 'Object' is any kind of game object, where @a@ should either be ThingData/RoomData
-- or Either ThingData RoomData
data Object specifics objData = Object
  { _objName :: !Text
  , _objDescription :: !Text
  , _objID :: !Entity
  , _objType :: !ObjType
  , _objCreationTime :: !Timestamp
  , _objSpecifics :: !(Either ObjectSpecifics specifics)
  , _objData :: !objData
  } deriving stock (Generic, Show)

instance Eq (Object specifics objData) where
  (==) = eqObject

-- | We extract this because we can compare objects with different data payloads.
eqObject
  :: Object specifics objData
  -> Object specifics objData'
  -> Bool
eqObject a b = _objID a == _objID b

-- | A 'TimestampedObject' is an object which has been cached at time '_tsCacheStamp'
-- and contains a function to update it given the state of the world. For instance,
-- this allows descriptions to be dynamic.
data TimestampedObject w specifics objData = TimestampedObject
  { _tsCachedObject :: !(Object specifics objData)
  , _tsCacheStamp :: !Timestamp
  , _tsUpdateFunc :: ObjectUpdate w specifics objData
  }

-- | Function to update an object
newtype ObjectUpdate w specifics objData = ObjectUpdate
  { updateObject :: forall m. (MonadReader w m) => Object specifics objData -> m (Object specifics objData)
  }
  
makeLenses ''TimestampedObject
-- | Update a cached object at a specified time
updateCachedObject :: 
  TimestampedObject w s d
  -> Object s d
  -> Timestamp
  -> TimestampedObject w s d
updateCachedObject ts n t = ts & set tsCachedObject n
                               & set tsCacheStamp t

-- | A lens to reify (and therefore also set) an object, but without updating on get.
objectL :: 
  Timestamp
  -> Lens' (AbstractObject w s d) (Object s d)
objectL t = lens
  (\case
    StaticObject o -> o
    DynamicObject (TimestampedObject o _ _) -> o)
  (\o n -> case o of
    StaticObject _ -> StaticObject n
    DynamicObject ts -> DynamicObject (updateCachedObject ts n t)
  )



-- | An abstract object is either a static object (which does not need to update itself)
-- or a timestamped object. Whilst this is what is stored internally, you shouldn't
-- need to pass these around; instead reify the object with 'reifyObject'.
data AbstractObject w s d
  = DynamicObject (TimestampedObject w s d)
  | StaticObject (Object s d)

type Thing s = Object s ThingData
type Room s = Object s RoomData
type AnyObject s = Object s (Either ThingData RoomData)

type AbstractThing s = AbstractObject s ThingData
type AbstractRoom s = AbstractObject s RoomData
type AnyAbstractObject s = AbstractObject s (Either ThingData RoomData)

