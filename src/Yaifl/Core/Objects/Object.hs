-- ~\~ language=Haskell filename=src/Yaifl/Core/Objects/Object.hs
-- ~\~ begin <<lit/worldmodel/objects/objects.md|src/Yaifl/Core/Objects/Object.hs>>[0] project://lit/worldmodel/objects/objects.md:11
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Core.Objects.Object (
  -- * Types
  ObjType(..)
  , Object(..)
  , Thing
  , Room
  , AnyObject
  , CanBeAny(..)
  -- * Object Helpers
  , objectEquals
  , isType
  -- * Lenses
  , objName, objDescription, objID, objType
  , objCreationTime, objSpecifics, objData
  -- * Prisms
  , _Room, _Thing ) where


import Yaifl.Core.Common ( WMObjSpecifics, Timestamp, HasID(..), Entity, typeDAG, Metadata )
import Yaifl.Core.Objects.ObjectData ( RoomData, ThingData )
import Cleff.State
import Data.Set (member)

-- ~\~ begin <<lit/worldmodel/objects/objects.md|obj-type>>[0] project://lit/worldmodel/objects/objects.md:133
newtype ObjType = ObjType
  { unObjType :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsList, IsString, Monoid, Semigroup)

isType ::
  forall wm es d.
  State (Metadata wm) :> es
  => Object wm d
  -> Text
  -> Eff es Bool
isType o = isTypeInternal (unObjType $ _objType o)
  where
    isTypeInternal ::
      Text
      -> Text
      -> Eff es Bool
    isTypeInternal obj e' = do
      td <- use $ typeDAG % at obj
      case td of
        Nothing -> error "" -- noteError ("Found no type entry for " <> obj) >> return False
        Just iv ->
          if
            e' `member` iv || obj == e'
          then
            return True
          else
            anyM (`isTypeInternal` e') iv

-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/objects.md|thing-room-anyobject>>[0] project://lit/worldmodel/objects/objects.md:147
type Thing wm = Object wm ThingData
type Room wm = Object wm (RoomData wm)
type AnyObject wm = Object wm (Either ThingData (RoomData wm))
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/objects.md|obj-definition>>[0] project://lit/worldmodel/objects/objects.md:52
data Object wm objData = Object
  { _objName :: !Text
  , _objDescription :: !Text
  , _objID :: !Entity
  , _objType :: !ObjType
  , _objCreationTime :: !Timestamp
  , _objSpecifics :: !(Maybe (WMObjSpecifics wm))
  , _objData :: !objData
  } deriving stock (Generic)

deriving stock instance (Show (WMObjSpecifics wm), Show d) => Show (Object wm d)
deriving stock instance (Read (WMObjSpecifics wm), Read d) => Read (Object wm d)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/objects.md|obj-hasid>>[0] project://lit/worldmodel/objects/objects.md:81
instance HasID (Object wm d) where
  getID = _objID
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/objects.md|obj-eq>>[0] project://lit/worldmodel/objects/objects.md:88
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
-- ~\~ begin <<lit/worldmodel/objects/objects.md|obj-functor>>[0] project://lit/worldmodel/objects/objects.md:105
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

-- ~\~ begin <<lit/worldmodel/objects/objects.md|obj-prisms>>[0] project://lit/worldmodel/objects/objects.md:164
_Room :: Prism' (AnyObject wm) (Room wm)
_Room = prism' (fmap Right) (traverse rightToMaybe)

_Thing :: Prism' (AnyObject wm) (Thing wm)
_Thing = prism' (fmap Left) (traverse leftToMaybe)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/objects.md|can-be-any>>[0] project://lit/worldmodel/objects/objects.md:174
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
-- ~\~ end
