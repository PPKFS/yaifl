{-|
Module      : Yaifl.Metadata
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Centralized game configuration and runtime state management.

This module defines Yaifl's metadata system, which serves as the central repository
for game-wide configuration, state, and utilities that are independent of the
specific world model. The metadata system is designed to:

1. **Centralize configuration**: Store all game settings in one place
2. **Manage game state**: Track runtime information like current player and stage
3. **Handle errors**: Provide phase-appropriate error reporting
4. **Support construction**: Assist with game building and verification
5. **Enable querying**: Offer utilities for object type and kind operations

The metadata system is lightweight by design, containing only information that:
- Is independent of the world model (no dynamic game objects)
- Applies globally across the game
- Needs to be accessible throughout the game lifecycle

Key components:
- `Metadata`: Core record containing all metadata fields
- `RoomDescriptions`: Configuration for room description verbosity
- `CurrentStage`: Game lifecycle stage tracking (construction/verification/runtime)
- `AnalysisLevel`: Error checking and validation depth
- `WithMetadata`: Convenience type synonym for metadata-dependent effects
- Error handling: Phase-aware error reporting utilities
- Construction helpers: Guards and utilities for game building
- Object utilities: Kind checking and inheritance operations
- RNG: Random number generation utilities

Example usage:
@
  -- Access metadata in an effectful computation
  title <- use #title
  
  -- Modify metadata
  #oxfordCommaEnabled .= True
  
  -- Error handling
  noteError (const ()) "Something went wrong"
  
  -- Kind checking
  isContainer <- objectIsKind "container" someObject
@

The metadata system works closely with:
- `Yaifl.Effects.ObjectQuery` for object operations
- `Yaifl.KindGraph` for object kind hierarchies
- `Yaifl.Object.Kind` for object type information
- `Yaifl.Tag` for type-safe entity references

This module is foundational for Yaifl's architecture, providing the infrastructure
that supports the game's configuration, error handling, and runtime state management.
-}

module Yaifl.Metadata (
  -- * Metadata Components
  RoomDescriptions(..)
  , Timestamp(..)
  , CurrentStage(..)
  , AnalysisLevel(..)
  -- * Metadata
  , Metadata(..)
  , WithMetadata
  -- * Error handling
  , noteError
  , noteRuntimeError
  , traceGuard
  , setPostPromptSpacing
  -- * Construction helpers
  , whenConstructing
  , whenConstructingM
  , setTitle
  , isRuntime
  -- * Querying
  , isPlayer
  , whenPlayer
  -- ** Timestamps
  , getGlobalTime
  , tickGlobalTime
  , isKind
  , mapKindsOf

  , kindIsUnderstoodAs
  , kindPluralIsUnderstoodAs
  , random
  , randomR
  ) where


import Yaifl.Prelude

import Breadcrumbs

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.KindGraph
import qualified Data.Set as S
import System.Random ( StdGen, UniformRange, uniformR, Uniform, uniform )

-- | Room description verbosity configuration.
--
-- Controls how room descriptions are printed to the player, affecting the balance
-- between immersion and brevity. This setting influences when full room descriptions
-- are shown versus abbreviated versions.
--
-- Constructors:
-- - `SometimesAbbreviatedRoomDescriptions`: Full descriptions on first visit, abbreviated thereafter
-- - `AbbreviatedRoomDescriptions`: Always use abbreviated descriptions (except for explicit "look")
-- - `NoAbbreviatedRoomDescriptions`: Always use full descriptions
--
-- Example usage:
-- @
--   -- Set verbose room descriptions
--   #roomDescriptions .= NoAbbreviatedRoomDescriptions
--   
--   -- Check current setting
--   currentSetting <- use #roomDescriptions
-- @
--
-- This setting affects player experience by controlling description repetition,
-- which is important for maintaining immersion without overwhelming players
-- with repetitive text.
data RoomDescriptions =
  SometimesAbbreviatedRoomDescriptions -- ^ Print full descriptions when visiting a room for the first time only.
  | AbbreviatedRoomDescriptions -- ^ Never print full descriptions (except when looking).
  | NoAbbreviatedRoomDescriptions -- ^ Always print full descriptions.
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

instance Display RoomDescriptions where
  displayBuilder SometimesAbbreviatedRoomDescriptions = "Sometimes abbreviated"
  displayBuilder NoAbbreviatedRoomDescriptions = "Never abbreviated"
  displayBuilder AbbreviatedRoomDescriptions = "Always abbreviated"

-- | Game lifecycle stage tracking.
--
-- Represents the current phase in Yaifl's build-verify-run lifecycle.
-- This distinction is crucial for appropriate error handling and behavior:
--
-- - `Construction`: Game is being built (object creation, world setup)
-- - `Verification`: Game is being validated (consistency checks, error detection)
-- - `Runtime`: Game is running normally (player interaction)
--
-- Different phases have different error handling requirements:
-- - Construction: Errors may be recoverable
-- - Verification: Errors should be reported but not crash
-- - Runtime: Errors should be user-friendly
--
-- Example:
-- @
--   -- Check if we're at runtime
--   isRuntime <- use #currentStage
--   when (isRuntime == Runtime) $ do
--     -- Runtime-specific behavior
-- @
--
-- This type enables phase-appropriate behavior throughout the codebase.
data CurrentStage = Construction | Verification | Runtime
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | Analysis and validation depth configuration.
--
-- Controls how thorough the game's internal consistency checking and error detection
-- should be. Higher levels perform more comprehensive checks but may impact performance.
--
-- Levels (from least to most thorough):
-- - `None`: Minimal checking (production mode)
-- - `Low`: Basic consistency checks
-- - `Medium`: Comprehensive validation
-- - `High`: Extensive checking with detailed reporting
-- - `Maximal`: All possible checks (development/debugging mode)
--
-- TODO: Document specific checks performed at each level
--
-- Example:
-- @
--   -- Enable maximal analysis during development
--   #traceAnalysisLevel .= Maximal
--   
--   -- Check current analysis level
--   level <- use #traceAnalysisLevel
--   when (level >= High) $ performExpensiveCheck
-- @
--
-- This setting is particularly useful during development and testing to catch
-- potential issues early. In production, lower levels are typically used for
-- better performance.
data AnalysisLevel = None | Low | Medium | High | Maximal
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | All the misc values about the game are stored here so we can carry it around. Notably, this does not include
-- anything dynamic (actions, activities) or anything relying on the worldmodel (objects), which means this remains
-- lightweight and simple.
-- | Core metadata record containing game configuration and state.
--
-- This record serves as the central repository for all game-wide metadata.
-- It contains configuration settings, runtime state, and utilities that are
-- accessible throughout the game.
--
-- Fields are organized into logical groups:
--
-- **Game Identity:**
-- - `title`: The game's title
--
-- **Presentation Configuration:**
-- - `roomDescriptions`: Verbosity level for room descriptions
-- - `oxfordCommaEnabled`: Whether to use Oxford commas in lists
-- - `usePostPromptPbreak`: Whether to add paragraph breaks after prompts
--
-- **Game State:**
-- - `globalTime`: Global timestamp counter
-- - `darknessWitnessed`: Whether the player has seen dark room descriptions
-- - `currentPlayer`: The active player entity
-- - `currentStage`: Current lifecycle stage (construction/verification/runtime)
-- - `previousRoom`: Last room added (for implicit object placement)
-- - `firstRoom`: The game's starting room
--
-- **Error Handling:**
-- - `errorLog`: Accumulated errors for testing/reporting
-- - `traceAnalysisLevel`: Current analysis/validation depth
--
-- **Parser Configuration:**
-- - `parserMatchThreshold`: Confidence threshold for parser matches
--
-- **Object System:**
-- - `kindDAG`: Object kind hierarchy (inheritance relationships)
-- - `mentionedThings`: Objects referenced in the last look action
--
-- **Runtime State:**
-- - `bufferedInput`: Buffered player input
-- - `rng`: Random number generator
--
-- Example usage:
-- @
--   -- Access metadata fields
--   gameTitle <- use #title
--   currentPlayer <- use #currentPlayer
--   
--   -- Modify metadata
--   #oxfordCommaEnabled .= True
--   #traceAnalysisLevel .= High
-- @
--
-- Performance note: This record is designed to be lightweight and is passed
-- around via the `State` effect rather than being copied. Access is O(1)
-- for all fields thanks to the optics system.
data Metadata = Metadata
  { title :: Text -- ^ The title of the game.
  , roomDescriptions :: RoomDescriptions -- ^ See `RoomDescriptions`.
  , globalTime :: Timestamp -- ^ See `Timestamp`.
  , darknessWitnessed :: Bool -- ^ Whether the player has seen the description of a dark room.
  , currentPlayer :: TaggedEntity PersonTag -- ^ The ID of the current player.
  , currentStage :: CurrentStage -- ^ See `CurrentStage`.
  , previousRoom :: TaggedEntity RoomTag -- ^ The last room that was added during construction (to implicitly place new objects).
  , firstRoom :: TaggedEntity RoomTag -- ^ The starting room.
  , errorLog :: [Text] -- ^ We keep track of noted errors for testing reasons.
  , kindDAG :: Map ObjectKind ObjectKindInfo -- ^ A fairly ad-hoc way to mimic inheritance: we track them as tags with no data.
  , traceAnalysisLevel :: AnalysisLevel -- ^ See `AnalysisLevel`.
  , oxfordCommaEnabled :: Bool -- ^ should we use the oxford comma in lists?
  , parserMatchThreshold :: Double -- ^ at what cutoff should we consider something a parser match?
  , bufferedInput :: [Text]
  , mentionedThings :: S.Set (TaggedEntity ThingTag) -- ^ All the things we've talked about in the last looking action.
  , rng :: StdGen
  , usePostPromptPbreak :: Bool
  -- more to come I guess
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Metadata

-- | Convenience type synonym for effects requiring metadata.
--
-- This synonym combines the two most common effects needed when working with
-- metadata:
-- - `State Metadata`: Access to the metadata state
-- - `Breadcrumbs`: Logging and annotation capabilities
--
-- Using this synonym simplifies type signatures throughout the codebase:
--
-- @
--   -- Instead of:
--   someFunction :: (State Metadata :> es, Breadcrumbs :> es) => Eff es ()
--   
--   -- You can write:
--   someFunction :: WithMetadata es => Eff es ()
-- @
--
-- This is used extensively throughout Yaifl for any function that needs
-- access to game configuration or state.
type WithMetadata es = (State Metadata :> es, Breadcrumbs :> es)

-- | Record an error and continue execution.
--
-- Adds an error message to the error log and continues with a recovery value.
-- This is used for non-fatal errors that should be reported but shouldn't
-- crash the game.
--
-- The error is:
-- - Added to the `errorLog` for later reporting/testing
-- - Logged via `Breadcrumbs` for immediate visibility
-- - Used to compute a recovery value via the provided function
--
-- Parameters:
-- - `f`: Recovery function that takes the error message and returns a value
-- - `t`: Error message to record
--
-- Returns: The result of applying the recovery function to the error message
--
-- Example:
-- @
--   -- Handle a missing object gracefully
--   result <- noteError (const defaultValue) "Object not found"
--   -- Continue with defaultValue
-- @
--
-- This function is phase-agnostic and will record errors in all stages.
-- For runtime-specific error handling, see `noteRuntimeError`.
noteError ::
  WithMetadata es
  => (Text -> a) -- ^ Recovery function (error message -> result)
  -> Text -- ^ Error message to record
  -> Eff es a
noteError f t = do
  #errorLog %= (t:)
  addAnnotation t
  pure $ f t

-- | Record an error during runtime phase only.
--
-- Similar to `noteError`, but only records the error if the game is in the
-- `Runtime` stage. This is useful for errors that are expected during
-- construction but should be reported during actual gameplay.
--
-- The error handling follows the same pattern as `noteError`:
-- - Error is added to `errorLog`
-- - Error is logged via `Breadcrumbs`
-- - Recovery value is computed and returned
--
-- Parameters:
-- - `f`: Recovery function that takes the error message and returns a value
-- - `t`: Error message to record
--
-- Returns: The result of applying the recovery function to the error message
--
-- Example:
-- @
--   -- Only report missing objects during runtime
--   result <- noteRuntimeError (const defaultValue) "Object not found"
--   -- During construction: returns defaultValue silently
--   -- During runtime: records error and returns defaultValue
-- @
--
-- This enables phase-appropriate error handling where construction-time
-- issues (which may be intentional) don't clutter the error log, while
-- runtime issues (which represent real problems) are properly recorded.
noteRuntimeError ::
  WithMetadata es
  => (Text -> a) -- ^ Recovery function (error message -> result)
  -> Text -- ^ Error message to record
  -> Eff es a
noteRuntimeError f t = do
  whenM isRuntime $ do
    #errorLog %= (t:)
    addAnnotation t
  pure $ f t

-- | Retrieve the global timestamp.
getGlobalTime ::
  State Metadata :> es
  => Eff es Timestamp
getGlobalTime = use #globalTime

-- | Increase the global timestamp.
tickGlobalTime ::
  State Metadata :> es
  => Eff es ()
tickGlobalTime = #globalTime %= (+1)

-- | Modify the title of the game.
setTitle ::
  State Metadata :> es
  => Text -- ^ New title.
  -> Eff es ()
setTitle = (#title .=)

-- | Set whether to use paragraph breaks after prompts.
setPostPromptSpacing ::
  State Metadata :> es
  => Bool
  -> Eff es ()
setPostPromptSpacing = (#usePostPromptPbreak .=)

-- | Determine if we are in the construction phase or in runtime,
-- for the sake of things which should be errors or ignored during construction
-- but should loudly complain when actually playing the game.
isRuntime ::
  State Metadata :> es
  => Eff es Bool
isRuntime = (Runtime ==) <$> use #currentStage

-- | Run something only during construction, where the condition is monadic.
whenConstructingM ::
  State Metadata :> es
  => Eff es Bool
  -> Eff es ()
  -> Eff es ()
whenConstructingM cond =
  whenM (andM [do
    cs <- use #currentStage
    return $ cs == Construction, cond])

-- | Run something only during construction.
whenConstructing ::
  State Metadata :> es
  => Bool
  -> Eff es ()
  -> Eff es ()
whenConstructing cond =
  whenM (andM [do
    cs <- use #currentStage
    return $ cs == Construction, pure cond])

-- | A guard for construction or when the analysis level enabled is greater than the specified
-- level.
traceGuard ::
  State Metadata :> es
  => AnalysisLevel
  -> Eff es Bool
traceGuard lvl = ((lvl <=) <$> use #traceAnalysisLevel) ||^ (not <$> isRuntime)

-- | If some `Entity` represents the current player.
isPlayer ::
  HasEntity o
  => State Metadata :> es
  => o
  -> Eff es Bool
isPlayer o = (getEntity o ==) . getEntity <$> use #currentPlayer

-- | Execute an action only if the given entity is the current player.
whenPlayer ::
  HasEntity o
  => State Metadata :> es
  => o
  -> Eff es ()
  -> Eff es ()
whenPlayer o = whenM (isPlayer o)

-- | Determine whether an object is of a certain type. This is separate to anything on Haskell's side
-- and the type system.
isKind ::
  WithMetadata es
  => Is k A_Getter
  => LabelOptic' "objectType" k o ObjectKind
  => o -- ^ The object.
  -> ObjectKind -- ^ The type.
  -> Eff es Bool
isKind o = isKindInternal (o ^. #objectType)
  where
    isKindInternal ::
      WithMetadata es
      => ObjectKind
      -> ObjectKind
      -> Eff es Bool
    isKindInternal obj e' = do
      td <- gets $ preview (#kindDAG % at obj % _Just % #parentKinds)
      case td of
        Nothing -> noteError (const False) ("Found no type entry for " <> show obj)
        Just iv ->
          if
            e' `S.member` iv || obj == e'
          then
            return True
          else
            anyM (`isKindInternal` e') iv

-- | Map over all kinds of an object, including inherited kinds.
-- Applies the given function to each kind's information and returns a set of results.
mapKindsOf ::
  forall es k o a.
  WithMetadata es
  => Ord a
  => Is k A_Getter
  => LabelOptic' "objectType" k o ObjectKind
  => o -- ^ The object.
  -> (ObjectKindInfo -> a)
  -> Eff es (S.Set a)
mapKindsOf o f = mapKindsInternal (o ^. #objectType)
  where
  mapKindsInternal :: ObjectKind -> Eff es (S.Set a)
  mapKindsInternal ty = do
    td <- gets @Metadata $ preview (#kindDAG % at ty % _Just)
    case td of
      Nothing -> noteError (const $ S.fromList []) ("Found no kind entry for " <> show ty)
      Just oki ->
        if S.null (oki ^. #parentKinds)
        then pure $ S.fromList [f oki]
        else S.insert (f oki) . mconcat <$> mapM mapKindsInternal (S.toList (oki ^. #parentKinds))

-- | Add additional nouns that can be used to refer to objects of this kind.
kindIsUnderstoodAs ::
  WithMetadata es
  => ObjectKind
  -> [Text]
  -> Eff es ()
kindIsUnderstoodAs kind otherKinds =
  #kindDAG % at kind % _Just % #understandAs %= (otherKinds<>)

-- | Add additional plural nouns that can be used to refer to objects of this kind.
kindPluralIsUnderstoodAs ::
  WithMetadata es
  => ObjectKind
  -> [Text]
  -> Eff es ()
kindPluralIsUnderstoodAs kind otherKinds =
  #kindDAG % at kind % _Just % #pluralUnderstandAs %= (otherKinds<>)

-- | Generate a random value within the specified range using the metadata RNG.
randomR ::
  UniformRange a
  => WithMetadata es
  => (a, a)
  -> Eff es a
randomR ran = do
  r <- use #rng
  let (res, rng2) = uniformR ran r
  #rng .= rng2
  pure res

-- | Generate a random value using the metadata RNG.
random ::
  Uniform a
  => WithMetadata es
  => Eff es a
random = do
  r <- use #rng
  let (res, rng2) = uniform r
  #rng .= rng2
  pure res