-- ~\~ language=Haskell filename=src/Yaifl/Objects/Object.hs
-- ~\~ begin <<lit/other_miscellania.md|src/Yaifl/Objects/Object.hs>>[0]

{-|
Module      : Yaifl.Objects.Object
Description : A game entity.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Yaifl.Objects.Object 
  ( -- * Types
    ObjType(..)
  , Object(..)
  , ObjectLike(..)
  , CanBeAny(..)
  , Thing
  , Room
  , AnyObject

  -- * Object Helpers
  , objectEquals
  , isType

  -- * Lenses
  , objName
  , objDescription
  , objID
  , objType
  , objCreationTime
  , objSpecifics
  , objData
  , containedBy
  , _Room
  , _Thing
  ) where

import Solitude
import Yaifl.Common
import Yaifl.ObjectSpecifics (ObjectSpecifics)
import Yaifl.Objects.ObjectData
import Yaifl.Objects.Missing
import {-# SOURCE #-} Yaifl.World
import Control.Monad.Except (liftEither, throwError)

-- ~\~ begin <<lit/worldmodel/objects/things.md|obj-type>>[0]
newtype ObjType = ObjType
  { unObjType :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsList, IsString, Monoid, Semigroup)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/things.md|thing-room-anyobject>>[0]
-- | Some of the (very rare) type aliases, just to make it easier to describe `Thing`s and `Room`s.
type Thing wm = Object wm ThingData
type Room wm = Object wm (RoomData wm)
type AnyObject wm = Object wm (Either ThingData (RoomData wm))
-- ~\~ end
-- | An 'Object' is any kind of game object, where @a@ should either be ThingData/RoomData
-- or Either ThingData RoomData

-- ~\~ begin <<lit/worldmodel/objects/things.md|obj-definition>>[0]
data Object wm objData = Object
  { _objName :: !Text
  , _objDescription :: !Text
  , _objID :: !Entity
  , _objType :: !ObjType
  , _objCreationTime :: !Timestamp
  , _objSpecifics :: !(Either ObjectSpecifics (WMObjSpecifics wm))
  , _objData :: !objData
  } deriving stock (Generic)

deriving stock instance (Show (WMObjSpecifics wm), Show d) => Show (Object wm d)
deriving stock instance (Read (WMObjSpecifics wm), Read d) => Read (Object wm d)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/things.md|obj-hasid>>[0]
instance HasID (Object wm d) where
  getID = _objID
-- ~\~ end

-- ~\~ begin <<lit/worldmodel/objects/things.md|obj-eq>>[0]
objectEquals :: 
  Object wm d
  -> Object wm d'
  -> Bool
objectEquals = (. _objID) . (==) . _objID

instance Eq (Object wm d) where
  (==) = objectEquals

-- | Maybe I'll need this instance for something or other? 
instance Ord (Object wm d) where
  compare = (. _objID) . compare . _objID
-- ~\~ end

makeLenses ''Object

-- ~\~ begin <<lit/worldmodel/objects/things.md|obj-functor>>[0]

instance Functor (Object wm) where
  fmap :: 
    (a -> b)
    -> Object wm a
    -> Object wm b
  fmap f = objData %~ f

instance Foldable (Object wm) where
  foldMap :: 
    (a -> m)
    -> Object wm a 
    -> m
  foldMap f = f . _objData

instance Traversable (Object wm) where
  traverse :: 
    Applicative f 
    => (a -> f b) 
    -> Object wm a 
    -> f (Object wm b)
  traverse f o = (\v -> o {_objData = v}) <$> f (_objData o)
-- ~\~ end



-- ~\~ begin <<lit/other_miscellania.md|other-stuff2>>[0]
{-
instance Prettify Text where
  prettify = id
instance Prettify (Object s d) where
  prettify Object{..} = _objName <> " (ID: " <>  show (unID _objID) <> ")\n" <> toStrict (pString (toString s)) where
    s = "{ Description = " <> _objDescription <>
        ", Type = " <> prettify _objType <>
        -- F.% ", Creation Time = " F.% F.stext
        ", Specifics = " <> prettify _objSpecifics <>
        ", Data = " <> prettify _objData
-}
containedBy :: forall wm. Lens' (Thing wm) Entity
containedBy = coercedTo @(Object wm ThingData) % objData % thingContainedBy

-- | Calculate whether one object type is a subclass of another
isType
  :: MonadReader (World wm) m
  -- => ObjectLike wm o
  => o
  -> ObjType
  -> m Bool
isType _ _ = return False
-- ~\~ end


-- ~\~ begin <<lit/worldmodel/objects/things.md|can-be-any>>[0]
class CanBeAny wm o where
  toAny :: o -> AnyObject wm
  fromAny :: AnyObject wm -> Maybe o

instance CanBeAny wm (Room wm) where
  toAny = review _Room
  fromAny = preview _Room

instance CanBeAny wm (Thing wm) where
  toAny = review _Thing
  fromAny = preview _Thing

instance CanBeAny wm (AnyObject wm) where
  toAny = id
  fromAny = Just
-- ~\~ end

-- ~\~ begin <<lit/worldmodel/objects/objectlike.md|objectlike>>[0]
  
class HasID o => ObjectLike wm o where
  getRoom :: (NoMissingObjects m, MonadWorld wm m) => o -> m (Room wm)
  default getRoom :: (NoMissingObjects m) => o -> m (Room wm)
  getRoom o = throwError $ MissingObject "Called getRoom on an object with no instance."  (getID o)
  
  getThing :: (NoMissingObjects m, MonadWorld wm m) => o -> m (Thing wm)
  default getThing :: (NoMissingObjects m) => o -> m (Thing wm)
  getThing o = throwError $ MissingObject "Called getThing on an object with no instance."  (getID o)

instance ObjectLike wm (Thing wm) where
  getThing = pure

instance ObjectLike wm (Room wm) where
  getRoom = pure

instance ObjectLike wm (AnyObject wm) where
  getThing t = liftEither
    (maybeToRight (MissingObject ("Tried to get a thing from " <> show (_objID t) <> " but it was a room.") (getID t))
      (preview _Thing t))
  getRoom t = liftEither
    (maybeToRight (MissingObject ("Tried to get a room from " <> show (_objID t) <> " but it was a thing.") (getID t))
      (preview _Room t))
-- ~\~ end
-- ~\~ end
