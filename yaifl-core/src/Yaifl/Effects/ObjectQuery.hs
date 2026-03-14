{-|
Module      : Yaifl.Effects.ObjectQuery
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

The ObjectQuery effect provides operations for reading, writing, and traversing
game objects (things, rooms, regions) in the world state.

This module defines:

- `ObjectQuery`: Core effect for object operations
- Object lookup and modification operations
- Object traversal utilities
- Error handling for missing objects
- Constraint synonyms for common effect combinations

The ObjectQuery system enables:
- Type-safe access to game objects
- Comprehensive object traversal and modification
- Graceful handling of missing object references
-}

module Yaifl.Effects.ObjectQuery
  ( -- * Effects
  ObjectQuery(..)
  , lookupThing
  , lookupRoom
  , lookupRegion
  , failHorriblyIfMissing
  , withoutMissingObjects
  , setThing
  , setRoom
  , setRegion
  , traverseRooms
  , traverseThings
  , traverseThings_
  , traverseRooms_
  , traverseRegions_
  , traverseRegions
  , generateEntity
  -- ** Type synonyms
  , WithoutMissingObjects
  , MissingObject(..)
  ) where

import Yaifl.Prelude

import Breadcrumbs
import Effectful.Error.Static
import Effectful.TH

import Yaifl.Entity
import Yaifl.Room.Kind
import Yaifl.Thing.Kind
import Yaifl.Metadata
import Yaifl.WorldModel
import Yaifl.Region.Kind

-- | Effect for reading/writing objects from the world.
data ObjectQuery (wm :: WorldModel) :: Effect where
  LookupThing :: HasEntity o => o -> ObjectQuery wm m (Either Text (Thing wm))
  LookupRoom :: HasEntity o => o -> ObjectQuery wm m (Either Text (Room wm))
  LookupRegion :: RegionEntity -> ObjectQuery wm m (Either Text (Region wm))

  GenerateEntity :: Bool -> ObjectQuery wm m Entity
  SetRoom :: Room wm -> ObjectQuery wm m ()
  SetThing :: Thing wm -> ObjectQuery wm m ()
  SetRegion :: Region wm -> ObjectQuery wm m ()

  TraverseThings :: (Thing wm -> m (Maybe (Thing wm))) -> ObjectQuery wm m [Thing wm]
  TraverseRooms :: (Room wm -> m (Maybe (Room wm))) -> ObjectQuery wm m [Room wm]
  TraverseRegions :: (Region wm -> m (Maybe (Region wm))) -> ObjectQuery wm m [Region wm]


makeEffect ''ObjectQuery

-- | Internal helper for object traversal that discards results.
traverseObjects_ ::
  ((a -> Eff es (Maybe a)) -> Eff es [a])
  -> (a -> Eff es (Maybe a))
  -> Eff es ()
traverseObjects_ selector f = void $ selector (\t -> f t >> return Nothing)

-- | Traverse all things, applying a modification function and discarding results.
traverseThings_ ::
  ObjectQuery wm :> es
  => (Thing wm -> Eff es (Maybe (Thing wm)))
  -> Eff es ()
traverseThings_ = traverseObjects_ traverseThings

-- | Traverse all rooms, applying a modification function and discarding results.
traverseRooms_ ::
  ObjectQuery wm :> es
  => (Room wm -> Eff es (Maybe (Room wm)))
  -> Eff es ()
traverseRooms_ = traverseObjects_ traverseRooms

-- | Traverse all regions, applying a modification function and discarding results.
traverseRegions_ ::
  ObjectQuery wm :> es
  => (Region wm -> Eff es (Maybe (Region wm)))
  -> Eff es ()
traverseRegions_ = traverseObjects_ traverseRegions

-- | Error payload for when an object is not present in the world.
-- However, most places currently just throw instead of doing any kind of error recovery...
data MissingObject = MissingObject
  { expected :: Text
  , entity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | Type synonym for reading/writing objects with a way to handle missing IDs.
type WithoutMissingObjects wm es = (Error MissingObject :> es, ObjectQuery wm :> es
  -- I hate putting this in here but it makes everything such a mess otherwise :(
  , Display (WMText wm), WithMetadata es)

-- | Execute an operation that may fail with missing objects, providing a recovery handler.
-- The handler typically returns a default value or empty result when objects are missing.
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

-- | Execute an operation that fails with an error if objects are missing.
-- Used when missing objects represent unrecoverable errors.
failHorriblyIfMissing ::
  HasCallStack
  => Breadcrumbs :> es
  => (HasCallStack => Eff (Error MissingObject ': es) a)
  -> Eff es a
failHorriblyIfMissing f = withoutMissingObjects f (\(MissingObject t o) -> do
  let msg = "the object with ID " <> show o <> " could not be found because " <> show t <> ". We are failing horribly and erroring out because we can't recover."
  addAnnotation msg
  error msg)