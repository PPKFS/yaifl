{-|
Module      : Yaifl.Model.Objects.Effects
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Effects for getting, setting, modifying, traversing over the `Object` collections and some
type synonyms for bundling common constraints together.
-}

module Yaifl.Model.Objects.Effects
  ( -- * Effects
  ObjectLookup(..)
  , lookupThing
  , lookupRoom
  , failHorriblyIfMissing
  , withoutMissingObjects
  , ObjectUpdate(..)
  , setThing
  , setRoom
  , ObjectTraverse(..)
  , traverseRooms
  , traverseThings
  , ObjectCreation(..)
  , generateEntity
  , addThingToWorld
  , addRoomToWorld
  -- ** Type synonyms
  , ObjectQuery
  , NoMissingObjects
  , MissingObject(..)
  , NoMissingRead
  , AddObjects
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display
import Effectful.Error.Static
import Effectful.TH

import Yaifl.Metadata
import Yaifl.Model.Object
import Yaifl.Model.Objects.Entity
import Yaifl.Model.WorldModel

-- | Effect for reading objects from the world.
data ObjectLookup (wm :: WorldModel) :: Effect where
  LookupThing :: HasID o => o -> ObjectLookup wm m (Either Text (Thing wm))
  LookupRoom :: HasID o => o -> ObjectLookup wm m (Either Text (Room wm))

-- | Effect for writing objects to the world.
data ObjectUpdate (wm :: WorldModel) :: Effect where
  SetRoom :: Room wm -> ObjectUpdate wm m ()
  SetThing :: Thing wm -> ObjectUpdate wm m ()

-- | Effect for traversing all objects in the world.
data ObjectTraverse (wm :: WorldModel) :: Effect where
  TraverseThings :: (Thing wm -> m (Maybe (Thing wm))) -> ObjectTraverse wm m ()
  TraverseRooms :: (Room wm -> m (Maybe (Room wm))) -> ObjectTraverse wm m ()

-- | Effect for creating new objects and adding them to the stores.
data ObjectCreation wm :: Effect where
  GenerateEntity :: Bool -> ObjectCreation wm m Entity
  AddThingToWorld :: Thing wm -> ObjectCreation wm m ()
  AddRoomToWorld :: Room wm -> ObjectCreation wm m ()

makeEffect ''ObjectCreation
makeEffect ''ObjectLookup
makeEffect ''ObjectUpdate
makeEffect ''ObjectTraverse

-- | Error payload for when an object is not present in the world.
-- However, most places currently just throw instead of doing any kind of error recovery...
data MissingObject = MissingObject
  { expected :: Text
  , entity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | Type synonym for reading/writing objects.
type ObjectQuery wm es = (ObjectLookup wm :> es, ObjectUpdate wm :> es)
-- | Type synonym for reading/writing objects with a way to handle missing IDs.
type NoMissingObjects wm es = (WithMetadata es, Error MissingObject :> es, ObjectQuery wm es, Display (WMSayable wm))
-- | Type synonym for reading objects with a way to handle missing IDs.
type NoMissingRead wm es = (Error MissingObject :> es, ObjectLookup wm :> es, WithMetadata es)

-- | Type synonym for adding new objects.
type AddObjects wm es = (
  ObjectCreation wm :> es
  , Display (WMSayable wm)
  , IsString (WMSayable wm)
  , State Metadata :> es
  , Pointed (WMObjSpecifics wm)
  , Breadcrumbs :> es, ObjectUpdate wm :> es, ObjectLookup wm :> es)

withoutMissingObjects ::
  HasCallStack
  => (HasCallStack => Eff (Error MissingObject ': es) a) -- ^ the block
  -> (HasCallStack => MissingObject -> Eff es a)  -- ^ the handler, which is basically always just "nothing"
  -> Eff es a
withoutMissingObjects f def = do
  r <- runError f
  case r of
    -- TODO: investigate what the callstack adds
    Left err' -> def (snd err')
    Right x -> return x

failHorriblyIfMissing ::
  HasCallStack
  => Breadcrumbs :> es
  => (HasCallStack => Eff (Error MissingObject ': es) a)
  -> Eff es a
failHorriblyIfMissing f = withoutMissingObjects f (\(MissingObject t o) -> do
  let msg = "the object with ID " <> show o <> " could not be found because " <> show t <> ". We are failing horribly and erroring out because we can't recover."
  addAnnotation msg
  error msg)