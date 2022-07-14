-- ~\~ language=Haskell filename=src/Yaifl/Core/Common.hs
-- ~\~ begin <<lit/misc/common.md|src/Yaifl/Core/Common.hs>>[0] project://lit/misc/common.md:10
{-# OPTIONS_GHC -Wno-orphans #-} --for the very disgusting display instance
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Common
  (-- * Datatypes
  Entity(..)
  , Store(..)
  , HasID(..)
  , Timestamp(..)
  , WorldModel(..)
  , RoomDescriptions(..)

  -- * Metadata
  , Metadata(..)
  , CurrentStage(..)
  , AnalysisLevel(..)
  , getGlobalTime
  , tickGlobalTime
  , previousRoom
  , firstRoom
  , whenConstructingM
  , whenConstructing
  , currentPlayer
  , title
  , setTitle
  , errorLog
  , noteError
  , typeDAG
  , darknessWitnessed
  , roomDescriptions
  , traceAnalysisLevel
  , traceGuard
  , isRuntime

  , ActionHandler(..)
  , parseAction

  -- * Some defaults
  , defaultVoidID
  , defaultPlayerID
  , emptyStore

  -- * Object querying
  , isThing
  , isRoom

    -- * Type family nonsense
  , WMObjSpecifics
  , WMValues
  , WMDirections
  , WMShow
  , WMRead
  , WMOrd
  , WMEq
  )
where

import Cleff.State ( State )
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Text.Pretty.Simple (pStringOpt, defaultOutputOptionsNoColor)
import Formatting.Buildable (Buildable(..))
import Prelude hiding (build)
import Data.Text.Lazy.Builder (toLazyText, fromText)
import Yaifl.Core.Say
import Yaifl.Core.Logger

instance {-# OVERLAPPABLE #-} Buildable a where
  build = const "No display instance"

-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|entity-def>>[0] project://lit/worldmodel/objects/entities-stores.md:7
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|thing-or-room>>[0] project://lit/worldmodel/objects/entities-stores.md:18
isThing ::
  (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom ::
  (HasID a)
  => a
  -> Bool
isRoom = not . isThing
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|has-id>>[0] project://lit/worldmodel/objects/entities-stores.md:34
class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id

instance Buildable Entity where
  build (Entity i) = "(ID: " <> show i <> ")"
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|base-ids>>[0] project://lit/worldmodel/objects/entities-stores.md:48
defaultVoidID :: Entity
defaultVoidID = Entity (-1)

defaultNothingID :: Entity
defaultNothingID = Entity 0

defaultPlayerID :: Entity
defaultPlayerID = Entity 1

-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|store-def>>[0] project://lit/worldmodel/objects/entities-stores.md:63
-- import qualified Data.EnumMap.Strict as EM
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Ord, Read, Foldable)

emptyStore :: Store a
emptyStore = Store EM.empty

pShowNice :: Buildable a => a -> Text
pShowNice = toStrict . pStringOpt defaultOutputOptionsNoColor . toString . toLazyText . build

instance Buildable (Store a) where
  build (Store s) = fromText $ pShowNice s
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|alter-store>>[0] project://lit/worldmodel/objects/entities-stores.md:80
alterEMF :: 
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a 
  -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

alterNewtypeEMF :: 
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> (nt -> EM.EnumMap k a)
  -> (EM.EnumMap k a -> nt)
  -> nt
  -> f nt
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|store-at>>[0] project://lit/worldmodel/objects/entities-stores.md:102
instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/entities-stores.md|store-instances>>[0] project://lit/worldmodel/objects/entities-stores.md:109
type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)
-- ~\~ end

-- ~\~ begin <<lit/worldmodel/state.md|room-descriptions>>[0] project://lit/worldmodel/state.md:104
data RoomDescriptions = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions 
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/state.md|timestamp>>[0] project://lit/worldmodel/state.md:142

newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

-- ~\~ end
-- ~\~ begin <<lit/worldmodel/typefamilies.md|world-model>>[0] project://lit/worldmodel/typefamilies.md:53
data WorldModel = WorldModel Type Type Type Type
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/typefamilies.md|world-model-families>>[0] project://lit/worldmodel/typefamilies.md:67
type family WMObjSpecifics (wm :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir o v) = objSpec

type family WMDirections (wm :: WorldModel) :: Type where
  WMDirections ('WorldModel objSpec dir o v) = dir 

type family WMValues (wm :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir o v) = o
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/typefamilies.md|world-model-constraints>>[0] project://lit/worldmodel/typefamilies.md:88
type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirections wm))
type WMShow wm = WMConstr Show wm
type WMRead wm = WMConstr Read wm
type WMOrd wm = WMConstr Ord wm
type WMEq wm = WMConstr Eq wm
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/state.md|world-metadata>>[0] project://lit/worldmodel/state.md:46

data Metadata (wm :: WorldModel) = Metadata
  { _title :: Text
  , _roomDescriptions :: RoomDescriptions
  , _dirtyTime :: Bool
  , _globalTime :: Timestamp
  , _darknessWitnessed :: Bool
  , _currentPlayer :: Entity
  , _currentStage :: CurrentStage
  , _previousRoom :: Entity
  , _firstRoom :: Entity
  , _errorLog :: [Text]
  , _typeDAG :: Map Text (Set Text)
  , _traceAnalysisLevel :: AnalysisLevel
  -- more to come I guess
  }

data CurrentStage = Construction | Verification | Runtime
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

data AnalysisLevel = None | Low | Medium | High | Maximal
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

makeLenses ''Metadata

noteError :: 
  State (Metadata wm) :> es 
  => Log :> es
  => (Text -> a)
  -> Text 
  -> Eff es a
noteError f t = do
  errorLog %= (t:)
  err t
  pure $ f t

getGlobalTime :: 
  State (Metadata wm) :> es 
  => Eff es Timestamp
getGlobalTime = use globalTime

tickGlobalTime ::
  Bool
  -> Eff es ()
tickGlobalTime _ = pass

setTitle :: 
  State (Metadata wm) :> es 
  => Text -- ^ New title.
  -> Eff es ()
setTitle = (title .=)

isRuntime :: 
  State (Metadata wm) :> es
  => Eff es Bool
isRuntime = (Runtime ==) <$> use currentStage

whenConstructingM :: 
  State (Metadata wm) :> es
  => Eff es Bool 
  -> Eff es () 
  -> Eff es ()
whenConstructingM cond = 
  whenM (andM [do
    cs <- use currentStage
    return $ cs == Construction, cond])

whenConstructing :: 
  State (Metadata wm) :> es
  => Bool
  -> Eff es () 
  -> Eff es ()
whenConstructing cond = 
  whenM (andM [do
    cs <- use currentStage
    return $ cs == Construction, pure cond])

traceGuard ::
  State (Metadata wm) :> es
  => AnalysisLevel
  -> Eff es Bool
traceGuard lvl = ((lvl <=) <$> use traceAnalysisLevel) ||^ (not <$> isRuntime)
data ActionHandler :: Effect where
  ParseAction :: Text -> ActionHandler m (Either Text Bool)

makeEffect ''ActionHandler
-- ~\~ end
-- ~\~ end
