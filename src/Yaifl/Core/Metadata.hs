{-|
Module      : Yaifl.Core.Metadata
Description : A record of various metadata that we keep around.
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com
-}

module Yaifl.Core.Metadata (
  -- * Metadata
    Metadata(..)
  -- ** Components
  , RoomDescriptions(..)
  , Timestamp(..)
  , CurrentStage(..)
  , AnalysisLevel(..)
  , ObjectType(..)
  , WithMetadata

  -- ** Error handling
  , noteError
  , traceGuard
  -- ** Construction
  , whenConstructing
  , whenConstructingM
  , setTitle
  , isRuntime
  , isPlayer
  -- ** Timestamps
  , getGlobalTime
  , tickGlobalTime
  ) where

import Solitude
import Effectful.Optics ( (.=), (%=), use )

import Yaifl.Core.Entity ( Entity, HasID (..) )
import Breadcrumbs
import Data.Text.Display

-- | Copy of Inform7's room description verbosity.
data RoomDescriptions =
  SometimesAbbreviatedRoomDescriptions -- ^ Print full descriptions when visiting a room for the first time only.
  | AbbreviatedRoomDescriptions -- ^ Never print full descriptions (except when looking).
  | NoAbbreviatedRoomDescriptions -- ^ Always print full descriptions.
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

instance Display RoomDescriptions where
  displayBuilder SometimesAbbreviatedRoomDescriptions = "Sometimes abbreviated"
  displayBuilder NoAbbreviatedRoomDescriptions = "Never abbreviated"
  displayBuilder AbbreviatedRoomDescriptions = "Always abbreviated"

-- | A `Timestamp` is used to date events or to keep track of when objects were last cached.
-- This probably introduces more overhead in checks than saves, but oh well.
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

-- | The status of the build-verify-run process.
data CurrentStage = Construction | Verification | Runtime
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | How much analysis should happen; this is a more lax version of property testing, where having a high analysis level
-- means that more checks will be done and errors printed.
-- TODO: actually decide on what happens at each tier.
data AnalysisLevel = None | Low | Medium | High | Maximal
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | See also `Yaifl.Core.Metadata.typeDAG`. An object type is just a string that has some relations to other types.
-- there is no data or polymorphism connected to a type, so it's very possible to call something a supporter without
-- having some supporter properties.
newtype ObjectType = ObjectType
  { unObjectType :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsString, Monoid, Semigroup)

-- | All the misc values about the game are stored here so we can carry it around. Notably, this does not include
-- anything dynamic (actions, activities) or anything relying on the worldmodel (objects), which means this remains
-- lightweight and simple.
data Metadata = Metadata
  { title :: Text -- ^ The title of the game.
  , roomDescriptions :: RoomDescriptions -- ^ See `RoomDescriptions`.
  , globalTime :: Timestamp -- ^ See `Timestamp`.
  , darknessWitnessed :: Bool -- ^ Whether the player has seen the description of a dark room.
  , currentPlayer :: Entity -- ^ The ID of the current player.
  , currentStage :: CurrentStage -- ^ See `CurrentStage`.
  , previousRoom :: Entity -- ^ The last room that was added during construction (to implicitly place new objects).
  , firstRoom :: Entity -- ^ The starting room.
  , errorLog :: [Text] -- ^ We keep track of noted errors for testing reasons.
  , typeDAG :: Map ObjectType (Set ObjectType) -- ^ A fairly ad-hoc way to mimic inheritance: we track them as tags with no data.
  , traceAnalysisLevel :: AnalysisLevel -- ^ See `AnalysisLevel`.
  -- more to come I guess
  } deriving stock (Generic)

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

getGlobalTime ::
  State Metadata :> es
  => Eff es Timestamp
getGlobalTime = use #globalTime

tickGlobalTime ::
  Bool
  -> Eff es ()
tickGlobalTime _ = pass

setTitle ::
  State Metadata :> es
  => Text -- ^ New title.
  -> Eff es ()
setTitle = (#title .=)

isRuntime ::
  State Metadata :> es
  => Eff es Bool
isRuntime = (Runtime ==) <$> use #currentStage

whenConstructingM ::
  State Metadata :> es
  => Eff es Bool
  -> Eff es ()
  -> Eff es ()
whenConstructingM cond =
  whenM (andM [do
    cs <- use #currentStage
    return $ cs == Construction, cond])

whenConstructing ::
  State Metadata :> es
  => Bool
  -> Eff es ()
  -> Eff es ()
whenConstructing cond =
  whenM (andM [do
    cs <- use #currentStage
    return $ cs == Construction, pure cond])

traceGuard ::
  State Metadata :> es
  => AnalysisLevel
  -> Eff es Bool
traceGuard lvl = ((lvl <=) <$> use #traceAnalysisLevel) ||^ (not <$> isRuntime)

isPlayer ::
  HasID o
  => State Metadata :> es
  => o
  -> Eff es Bool
isPlayer o = (getID o ==) <$> use #currentPlayer