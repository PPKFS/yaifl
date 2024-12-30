{-|
Module      : Yaifl.Core.Effects
Copyright   : (c) Avery 2023-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

Effects for getting, setting, modifying, traversing over the `Object` collections and some
type synonyms for bundling common constraints together.
-}

module Yaifl.Core.Effects
  ( -- * Effects
  ObjectLookup(..)
  , lookupThing
  , lookupRoom
  , lookupRegion
  , failHorriblyIfMissing
  , withoutMissingObjects
  , ObjectUpdate(..)
  , setThing
  , setRoom
  , setRegion
  , ObjectTraverse(..)
  , traverseRooms
  , traverseThings
  , traverseThings_
  , traverseRegions
  , generateEntity
  -- ** Type synonyms
  , ObjectQuery
  , NoMissingObjects
  , MissingObject(..)
  , NoMissingRead
  ) where

import Yaifl.Prelude

import Breadcrumbs
import Effectful.Error.Static
import Effectful.TH

import Yaifl.Core.Metadata
import Yaifl.Core.Entity
import Yaifl.Model.WorldModel
import Yaifl.Model.Kinds.Region
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Room

-- | Effect for reading objects from the world.
data ObjectLookup (wm :: WorldModel) :: Effect where
  LookupThing :: HasID o => o -> ObjectLookup wm m (Either Text (Thing wm))
  LookupRoom :: HasID o => o -> ObjectLookup wm m (Either Text (Room wm))
  LookupRegion :: RegionEntity -> ObjectLookup wm m (Either Text (Region wm))

-- | Effect for writing objects to the world.
data ObjectUpdate (wm :: WorldModel) :: Effect where
  GenerateEntity :: Bool -> ObjectUpdate wm m Entity
  SetRoom :: Room wm -> ObjectUpdate wm m ()
  SetThing :: Thing wm -> ObjectUpdate wm m ()
  SetRegion :: Region wm -> ObjectUpdate wm m ()

-- | Effect for traversing all objects in the world.
data ObjectTraverse (wm :: WorldModel) :: Effect where
  TraverseThings :: (Thing wm -> m (Maybe (Thing wm))) -> ObjectTraverse wm m [Thing wm]
  TraverseRooms :: (Room wm -> m (Maybe (Room wm))) -> ObjectTraverse wm m [Room wm]
  TraverseRegions :: (Region wm -> m (Maybe (Region wm))) -> ObjectTraverse wm m [Region wm]

makeEffect ''ObjectLookup
makeEffect ''ObjectUpdate
makeEffect ''ObjectTraverse

traverseThings_ ::
  ObjectTraverse wm :> es
  => (Thing wm -> Eff es (Maybe (Thing wm)))
  -> Eff es ()
traverseThings_ f = void $ traverseThings (\t -> f t >> return Nothing)

-- | Error payload for when an object is not present in the world.
-- However, most places currently just throw instead of doing any kind of error recovery...
data MissingObject = MissingObject
  { expected :: Text
  , entity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | Type synonym for reading/writing objects.
type ObjectQuery wm es = (ObjectLookup wm :> es, ObjectUpdate wm :> es)
-- | Type synonym for reading/writing objects with a way to handle missing IDs.
type NoMissingObjects wm es = (WithMetadata es, Error MissingObject :> es, ObjectQuery wm es, Display (WMText wm))
-- | Type synonym for reading objects with a way to handle missing IDs.
type NoMissingRead wm es = (Error MissingObject :> es, ObjectLookup wm :> es, WithMetadata es)

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