{-|
Module      : Yaifl.Core.Metadata
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

General game-specific metadata (user settings and configs, object type information,
and helpers for the construction phase). Should not be dependent on anything major
because most things rely on this in some form.
-}

module Yaifl.Core.Metadata (
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

import Breadcrumbs
import Data.Text.Display
import Yaifl.Prelude
import Yaifl.Core.Entity
import Yaifl.Core.Kinds.Object
import Yaifl.Model.ObjectKind
import qualified Data.Set as S
import System.Random ( StdGen, UniformRange, uniformR, Uniform, uniform )

-- | Whether the room descriptions should be printed verbosely sometimes, all the time, or never.
data RoomDescriptions =
  SometimesAbbreviatedRoomDescriptions -- ^ Print full descriptions when visiting a room for the first time only.
  | AbbreviatedRoomDescriptions -- ^ Never print full descriptions (except when looking).
  | NoAbbreviatedRoomDescriptions -- ^ Always print full descriptions.
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

instance Display RoomDescriptions where
  displayBuilder SometimesAbbreviatedRoomDescriptions = "Sometimes abbreviated"
  displayBuilder NoAbbreviatedRoomDescriptions = "Never abbreviated"
  displayBuilder AbbreviatedRoomDescriptions = "Always abbreviated"

-- | The status of the build-verify-run process.
data CurrentStage = Construction | Verification | Runtime
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | How much analysis should happen; this is a more lax version of property testing, where having a high analysis level
-- means that more checks will be done and errors printed.
-- TODO: actually decide on what happens at each tier.
data AnalysisLevel = None | Low | Medium | High | Maximal
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | All the misc values about the game are stored here so we can carry it around. Notably, this does not include
-- anything dynamic (actions, activities) or anything relying on the worldmodel (objects), which means this remains
-- lightweight and simple.
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
  -- more to come I guess
  } deriving stock (Generic)
makeFieldLabelsNoPrefix ''Metadata

-- | As basically everywhere where we need the `Metadata` probably uses logging, this type synonym
-- makes it a bit easier.
type WithMetadata es = (State Metadata :> es, Breadcrumbs :> es)

-- | Take note of an error (to be reported later) but continue execution.
noteError ::
  WithMetadata es
  => (Text -> a) -- ^ How to recover.
  -> Text -- ^ Error message.
  -> Eff es a
noteError f t = do
  #errorLog %= (t:)
  addAnnotation t
  pure $ f t

-- | Take note of an error if we're out of the building phase (to be reported later) but continue execution.
noteRuntimeError ::
  WithMetadata es
  => (Text -> a) -- ^ How to recover.
  -> Text -- ^ Error message.
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
  HasID o
  => State Metadata :> es
  => o
  -> Eff es Bool
isPlayer o = (getID o ==) . getID <$> use #currentPlayer

whenPlayer ::
  HasID o
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

kindIsUnderstoodAs ::
  WithMetadata es
  => ObjectKind
  -> [Text]
  -> Eff es ()
kindIsUnderstoodAs kind otherKinds =
  #kindDAG % at kind % _Just % #understandAs %= (otherKinds<>)

kindPluralIsUnderstoodAs ::
  WithMetadata es
  => ObjectKind
  -> [Text]
  -> Eff es ()
kindPluralIsUnderstoodAs kind otherKinds =
  #kindDAG % at kind % _Just % #pluralUnderstandAs %= (otherKinds<>)

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

random ::
  Uniform a
  => WithMetadata es
  => Eff es a
random = do
  r <- use #rng
  let (res, rng2) = uniform r
  #rng .= rng2
  pure res