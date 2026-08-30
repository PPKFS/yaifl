{-|
Module      : Yaifl.Metadata wm
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

General game-specific Metadata wm including user settings, configuration, object type information,
and construction phase helpers. This module serves as the central repository for game state
that doesn't depend on the world model or dynamic game elements.
-}

module Yaifl.Metadata (
  -- * Metadata wm Types
  RoomDescriptions(..)
  , Timestamp(..)
  , CurrentStage(..)
  , AnalysisLevel(..)
  -- * Metadata
  , StatusBar(..)
  , Score(..)
  , Metadata(..)
  , WithMetadata
  -- * Error Handling
  , noteError
  , noteRuntimeError
  , traceGuard
  , setPostPromptSpacing
  -- * Construction Helpers
  , whenConstructing
  , whenConstructingM
  , setTitle
  , setLeftStatusBar
  , setRightStatusBar
  , getScore
  , getScore'
  , getMaxScore
  , isRuntime
  , gameHasEnded
  -- * Querying
  , isPlayer
  , whenPlayer
  , getTurnCount
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
import Yaifl.WorldModel
-- | Room description verbosity configuration.
--
-- Controls how room descriptions are printed to the player, affecting the balance
-- between immersion and brevity.
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
--
-- - `Construction`: Game is being built (object creation, world setup)
-- - `Verification`: Game is being validated (consistency checks, error detection)
-- - `Runtime`: Game is running normally (player interaction)
data CurrentStage = Construction | Verification | BeforePlay | DuringPlay | Ended
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | Analysis and validation depth configuration.
--
-- Controls how thorough the game's internal consistency checking and error detection
-- should be.
--
-- Levels (from least to most thorough):
-- - `None`: Minimal checking (production mode)
-- - `Low`: Basic consistency checks
-- - `Medium`: Comprehensive validation
-- - `High`: Extensive checking with detailed reporting
-- - `Maximal`: All possible checks (development/debugging mode)
data AnalysisLevel = None | Low | Medium | High | Maximal
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

data StatusBar wm = StatusBar
  { leftStatusBar :: WMText wm
  , rightStatusBar :: WMText wm
  } deriving stock (Generic)

data Score = Score
  { currentScore :: Int
  , maxScore :: Maybe Int
  , usingScore :: Bool
  } deriving stock (Eq, Show, Read, Ord, Generic)
-- | All the misc values about the game are stored here so we can carry it around. Notably, this does not include
-- anything dynamic (actions, activities) which means this remains lightweight and simple.
--
data Metadata wm = Metadata
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
  , usePostPromptPbreak :: Bool -- ^ Whether to add paragraph breaks after prompts
  , statusBar :: StatusBar wm
  , score :: Score
  , turnCount :: Int
  , everywhereBackdrops :: S.Set ThingEntity
  -- more to come I guess
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Metadata

-- |
-- This synonym combines the two most common effects for general monadic functions:
-- - `State Metadata wm`: Access to the Metadata wm state
-- - `Breadcrumbs`: Logging and annotation capabilities
type WithMetadata wm es = (State (Metadata wm) :> es, Breadcrumbs :> es)

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
-- This function is phase-agnostic and will record errors in all stages.
-- For runtime-specific error handling, see `noteRuntimeError`.
noteError ::
  HasCallStack
  => WithMetadata wm es
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
noteRuntimeError ::
  WithMetadata wm es
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
  State (Metadata wm) :> es
  => Eff es Timestamp
getGlobalTime = use #globalTime

-- | Increase the global timestamp.
tickGlobalTime ::
  State (Metadata wm) :> es
  => Eff es ()
tickGlobalTime = #globalTime %= (+1)

-- | Modify the title of the game.
setTitle ::
  State (Metadata wm) :> es
  => Text -- ^ New title.
  -> Eff es ()
setTitle = (#title .=)

setLeftStatusBar ::
  State (Metadata wm) :> es
  => WMText wm
  -> Eff es ()
setLeftStatusBar = (#statusBar % #leftStatusBar .=)

setRightStatusBar ::
  State (Metadata wm) :> es
  => WMText wm
  -> Eff es ()
setRightStatusBar = (#statusBar % #rightStatusBar .=)
-- | Set whether to use paragraph breaks after prompts.
setPostPromptSpacing ::
  State (Metadata wm) :> es
  => Bool
  -> Eff es ()
setPostPromptSpacing = (#usePostPromptPbreak .=)

-- | Determine if we are in the construction phase or in runtime,
-- for the sake of things which should be errors or ignored during construction
-- but should loudly complain when actually playing the game.
isRuntime ::
  State (Metadata wm) :> es
  => Eff es Bool
isRuntime = (DuringPlay ==) <$> use #currentStage

gameHasEnded ::
  State (Metadata wm) :> es
  => Eff es Bool
gameHasEnded = (Ended ==) <$> use #currentStage

-- | Run something only during construction, where the condition is monadic.
whenConstructingM ::
  State (Metadata wm) :> es
  => Eff es Bool
  -> Eff es ()
  -> Eff es ()
whenConstructingM cond =
  whenM (andM [do
    cs <- use #currentStage
    return $ cs == Construction, cond])

-- | Run something only during construction.
whenConstructing ::
  State (Metadata wm) :> es
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
  State (Metadata wm) :> es
  => AnalysisLevel
  -> Eff es Bool
traceGuard lvl = ((lvl <=) <$> use #traceAnalysisLevel) ||^ (not <$> isRuntime)

-- | If some object represents the current player.
isPlayer ::
  HasEntity o
  => State (Metadata wm) :> es
  => o
  -> Eff es Bool
isPlayer o = (getEntity o ==) . getEntity <$> use #currentPlayer

-- | Execute an action only if the given entity is the current player.
whenPlayer ::
  HasEntity o
  => State (Metadata wm) :> es
  => o
  -> Eff es ()
  -> Eff es ()
whenPlayer o = whenM (isPlayer o)

-- | Determine whether an object is of a certain type. This is separate to anything on Haskell's side
-- and the type system.
isKind ::
  WithMetadata wm es
  => Is k A_Getter
  => LabelOptic' "objectType" k o ObjectKind
  => o -- ^ The object.
  -> ObjectKind -- ^ The type.
  -> Eff es Bool
isKind o = isKindInternal (o ^. #objectType)
  where
    isKindInternal ::
      WithMetadata wm es
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
  forall wm es k o a.
  WithMetadata wm es
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
    td <- gets @(Metadata wm) $ preview (#kindDAG % at ty % _Just)
    case td of
      Nothing -> noteError (const $ S.fromList []) ("Found no kind entry for " <> show ty)
      Just oki ->
        if S.null (oki ^. #parentKinds)
        then pure $ S.fromList [f oki]
        else S.insert (f oki) . mconcat <$> mapM mapKindsInternal (S.toList (oki ^. #parentKinds))

-- | Add additional nouns that can be used to refer to objects of this kind.
kindIsUnderstoodAs ::
  WithMetadata wm es
  => ObjectKind
  -> [Text]
  -> Eff es ()
kindIsUnderstoodAs kind otherKinds =
  #kindDAG % at kind % _Just % #understandAs %= (otherKinds<>)

-- | Add additional plural nouns that can be used to refer to objects of this kind.
kindPluralIsUnderstoodAs ::
  WithMetadata wm es
  => ObjectKind
  -> [Text]
  -> Eff es ()
kindPluralIsUnderstoodAs kind otherKinds =
  #kindDAG % at kind % _Just % #pluralUnderstandAs %= (otherKinds<>)

-- | Generate a random value within the specified range using the Metadata wm RNG.
randomR ::
  UniformRange a
  => WithMetadata wm es
  => (a, a)
  -> Eff es a
randomR ran = do
  r <- use #rng
  let (res, rng2) = uniformR ran r
  #rng .= rng2
  pure res

-- | Generate a random value using the Metadata wm RNG.
random ::
  Uniform a
  => WithMetadata wm es
  => Eff es a
random = do
  r <- use #rng
  let (res, rng2) = uniform r
  #rng .= rng2
  pure res

getScore ::
  WithMetadata wm es
  => Eff es (Maybe Int)
getScore = do
  s <- use #score
  if usingScore s then return $ Just (currentScore s) else return Nothing

getScore' ::
  WithMetadata wm es
  => Eff es Int
getScore' = do
  s <- use #score
  if usingScore s then return $ currentScore s else return 0

getMaxScore ::
  WithMetadata wm es
  => Eff es (Maybe Int)
getMaxScore = do
  s <- use #score
  if usingScore s then return $ (maxScore s) else return Nothing
getTurnCount ::
  WithMetadata wm es
  => Eff es Int
getTurnCount = use #turnCount