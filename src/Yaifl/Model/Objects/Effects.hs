{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Objects.Effects
  ( ObjectQuery
  , ObjectLookup(..)
  , ObjectUpdate(..)
  , ObjectTraverse(..)

  , NoMissingObjects
  , NoMissingRead
  , ObjectRead

  , MissingObject(..)

  , lookupThing
  , lookupRoom
  , setThing
  , setRoom
  , traverseRooms
  , traverseThings

  , ObjectCreation(..)
  , AddObjects
  , generateEntity
  , addThingToWorld
  , addRoomToWorld
  ) where

import Breadcrumbs
import Data.Text.Display
import Effectful.Error.Static
import Effectful.TH
import Solitude
import Yaifl.Metadata
import Yaifl.Model.Entity
import Yaifl.Model.Object
import Yaifl.Model.WorldModel

data ObjectLookup (wm :: WorldModel) :: Effect where
  LookupThing :: HasID o => o -> ObjectLookup wm m (Either Text (Thing wm))
  LookupRoom :: HasID o => o -> ObjectLookup wm m (Either Text (Room wm))

data ObjectUpdate (wm :: WorldModel) :: Effect where
  SetRoom :: Room wm -> ObjectUpdate wm m ()
  SetThing :: Thing wm -> ObjectUpdate wm m ()

data ObjectTraverse (wm :: WorldModel) :: Effect where
  TraverseThings :: (Thing wm -> m (Maybe (Thing wm))) -> ObjectTraverse wm m ()
  TraverseRooms :: (Room wm -> m (Maybe (Room wm))) -> ObjectTraverse wm m ()

data ObjectCreation wm :: Effect where
  GenerateEntity :: Bool -> ObjectCreation wm m Entity
  AddThingToWorld :: Thing wm -> ObjectCreation wm m ()
  AddRoomToWorld :: Room wm -> ObjectCreation wm m ()

makeEffect ''ObjectCreation
makeEffect ''ObjectLookup
makeEffect ''ObjectUpdate
makeEffect ''ObjectTraverse

data MissingObject = MissingObject
  { expected :: Text
  , entity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

type ObjectQuery wm es = (ObjectLookup wm :> es, ObjectUpdate wm :> es)
type NoMissingObjects wm es = (WithMetadata es, Error MissingObject :> es, ObjectQuery wm es, Display (WMSayable wm))
type NoMissingRead wm es = (Error MissingObject :> es, ObjectLookup wm :> es, WithMetadata es)
type ObjectRead wm es = (ObjectLookup wm :> es, WithMetadata es)

type AddObjects wm es = (
  ObjectCreation wm :> es
  , Display (WMSayable wm)
  , IsString (WMSayable wm)
  , State Metadata :> es
  , Pointed (WMObjSpecifics wm)
  , Breadcrumbs :> es, ObjectUpdate wm :> es, ObjectLookup wm :> es)