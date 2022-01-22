module Yaifl.Types where

import Yaifl.Prelude
import Yaifl.Messages
import qualified Data.EnumSet as ES
import qualified Data.EnumMap.Strict as EM
import Katip
import qualified Data.Aeson as A
import qualified Data.Text.Lazy.Builder as B
import Katip.Format.Time (formatAsLogTime)
import Language.Haskell.TH
import qualified Data.Text as T
import Data.Aeson.Types hiding (Object)
import qualified Data.Text.Lazy.Builder as TLB
import GHC.Stack (srcLocFile, srcLocPackage, srcLocModule, srcLocStartLine, srcLocStartCol, srcLocEndLine, srcLocEndCol)
import Control.Monad.Except (MonadError)


class Default e where
  blank :: e

-- | An 'Entity' is an integer ID that is used to reference between objects. The phantom type
-- is for tagging it as a room, thing, or either
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic, Eq)
    deriving newtype (Num, Enum, Ord, Real, Integral)

-- | ObjTypes make a DAG that approximates inheritance; for instance, we may only care
-- that an object *is* a kind of food, but we don't necessarily know what the @a@ is
-- or looks like.
newtype ObjType = ObjType
  { unObjType :: Text }
  deriving stock (Show)

-- | A 'Store' is a map from 'Entity's to @a@s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  }  deriving stock (Show)

instance Default (Store a) where
  blank = Store EM.empty

type StoreLens' s d = (Lens' (World s) (Store (AbstractObject s d)))

instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)

type LocalePriorities s = Store (LocaleInfo s)

data LocaleInfo s = LocaleInfo
  { _priority :: Int
  , _localeObject :: AnyObject s
  , _isMentioned :: Bool
  }
-- | For now, a timestamp is simply an integer. The timestamp is updated whenever some
-- modification is made to the 'World'; therefore it does not directly correspond to
-- some sort of in-game turn counter. For example, throwing an object would result in
-- multiple timestamp jumps (an object moving, potential interactions on it hitting
-- something) whereas a sequence of 10 look actions will not (as the world does not
-- change). This is primarily used to ensure we can cache updates of objects that
-- change properties (e.g. strings).
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Generic, Eq)
    deriving newtype (Num, Enum, Ord, Real, Integral)

instance Default Timestamp where
  blank = Timestamp 0

-- | An 'Object' is any kind of game object, where @a@ should either be ThingData/RoomData
-- or Either ThingData RoomData
data Object b a = Object
  { _objName :: !Text
  , _objDescription :: !Text
  , _objID :: !Entity
  , _objType :: !ObjType
  , _objCreationTime :: !Timestamp
  , _objSpecifics :: !(Either ObjectSpecifics b)
  , _objData :: !a
  } deriving stock (Generic, Show)

instance Eq (Object b a) where
  (==) = eqObject

eqObject
  :: Object s d
  -> Object s e
  -> Bool
eqObject a b = _objID a == _objID b

-- | A 'TimestampedObject' is an object which has been cached at time '_tsCacheStamp'
-- and contains a function to update it given the state of the world. For instance,
-- this allows descriptions to be dynamic.
data TimestampedObject s d = TimestampedObject
  { _tsCachedObject :: !(Object s d)
  , _tsCacheStamp :: !Timestamp
  , _tsUpdateFunc :: ObjectUpdate s d
  }

-- | Function to update an object
newtype ObjectUpdate s d = ObjectUpdate
  { updateObject :: forall m. (MonadWorldRO s m) => Object s d -> m (Object s d)
  }

type Thing s = Object s ThingData
type Room s = Object s RoomData
type AnyObject s = Object s (Either ThingData RoomData)

type AbstractThing s = AbstractObject s ThingData
type AbstractRoom s = AbstractObject s RoomData
type AnyAbstractObject s = AbstractObject s (Either ThingData RoomData)

-- | An abstract object is either a static object (which does not need to update itself)
-- or a timestamped object. Whilst this is what is stored internally, you shouldn't
-- need to pass these around; instead reify the object with 'reifyObject'.
data AbstractObject s d
  = DynamicObject (TimestampedObject s d)
  | StaticObject (Object s d)

-- | Whether a room has an intrinsic light-ness. This isn't equivalent to whether a
-- room is currently dark - for instance, a cave may have light (if the player has a
-- lantern) but the cave will be Dark.
data Darkness = Lighted | Dark deriving (Eq, Show)

-- | Whether a room has been visited before or not.
data IsVisited = Visited | Unvisited deriving (Eq, Show)

-- | The connections from a one room to another, stored by direction ID.
type MapConnections = Store Entity

-- | An abstract grouping of rooms.
type ContainingRegion = Maybe Entity

-- | Details for room objects. This is anything which is...well, a room. Nontangible.
data RoomData = RoomData
  { _roomIsVisited :: !IsVisited
  , _roomDarkness :: !Darkness
  , _roomMapConnections :: !MapConnections
  , _roomContainingRegion :: !ContainingRegion
  , _roomEnclosing :: !Enclosing
  } deriving stock (Generic, Show)

defaultVoidID :: Entity
defaultVoidID = Entity (-1)

defaultPlayerID :: Entity
defaultPlayerID = Entity 1

-- | Whether a thing is inherently lit or not. This counts for lighting up spaces.
data ThingLit = Lit | NotLit deriving (Eq, Show)

-- | Details for things. This is anything tangible.
data ThingData = ThingData
  { _thingContainedBy :: !Entity
  , _thingLit :: !ThingLit
  } deriving stock (Generic, Show)

data Enclosing = Enclosing
  { _enclosingContains :: ES.EnumSet Entity
  , _enclosingCapacity :: Maybe Int
  } deriving stock (Show, Eq)

instance Default Enclosing where
  blank = Enclosing ES.empty Nothing

instance Default ThingData where
  blank = ThingData defaultVoidID NotLit

instance Default RoomData where
  blank = RoomData Unvisited Lighted blank Nothing blank

data Opacity = Opaque | Transparent deriving stock (Eq, Show)
data Enterable = Enterable | NotEnterable deriving stock (Eq, Show)
data Openable = Open | Closed deriving stock (Eq, Show)

data Container = Container
  { _containerOpacity :: Opacity
  , _containerEnclosing :: Enclosing
  , _containerOpenable :: Openable
  , _containerEnterable :: Enterable
  } deriving stock (Eq, Show)

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container deriving stock (Show)

-- | Again lifted directly from Inform; this sets whether to always print room
-- descriptions (No..) even if the room is visited, to only print them on the first
-- entry (Sometimes..) or never.
data RoomDescriptions
  = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions
  deriving (Eq, Show)

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule s v r = Rule
  { _ruleName :: !Text
  , _runRule :: forall m. NoMissingObjects s m => MonadWorld s m => v -> m (v, Maybe r)
  }

instance Default (Text -> Rule s v r) where
  blank n = Rule n (\v -> do
    warn $ bformat (stext %! " needs implementing") n
    return (v, Nothing))

--instance HasBuffer (World s) 'LogBuffer => Default (String -> Rule s v r) where
--  blank = blank . toText

-- | A 'Rulebook' is a composition of functions ('Rule's) with short-circuiting (if
-- a Just value is returned) over an object universe `o`, input arguments `ia`, variables `v`
-- and returns an `r`.
data Rulebook s ia v r where
  Rulebook ::
    { _rbName :: !Text
    , _rbDefaultOutcome :: !(Maybe r)
    , _rbParseArguments :: !(ParseArguments s ia v)
    , _rbRules :: ![Rule s v r]
    } -> Rulebook s ia v r

instance Default (Text -> Rulebook s v v r) where
  blank n = Rulebook n Nothing (ParseArguments (return . Just)) []

blankRulebook ::
  Text 
  -> Rulebook s v v r
blankRulebook = blank
-- | Arguments for an action, activity, or rulebook. These are parameterised over
-- the closed 's' universe and the variables, which are either unknown
-- (see 'UnverifiedArgs') or known (concrete instantation).
data Args s v = Args
  { _argsSource :: !(Maybe (AnyObject s))
  , _argsVariables :: !v
  , _argsTimestamp :: !Timestamp
  }

-- | Before 'Args' are parsed, the variables are a list of objects.
newtype UnverifiedArgs s = UnverifiedArgs
  { unArgs :: Args s [AnyObject s]
  }

instance Default (Timestamp -> UnverifiedArgs s) where
  blank = UnverifiedArgs . Args Nothing []

instance Default (UnverifiedArgs s) where
  blank = UnverifiedArgs $ Args Nothing [] 0

-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook s v = Rulebook s (Args s v) (Args s v) Bool

-- | A `StandardRulebook` is one which expects to verify its own arguments.
type StandardRulebook s v r = Rulebook s (UnverifiedArgs s) v r

newtype ParseArguments s ia v = ParseArguments
  { runParseArguments :: forall m. (NoMissingObjects s m, MonadWorld s m) => ia -> m (Maybe v)
  }

type ActionParseArguments s v = ParseArguments s (UnverifiedArgs s) v

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action s where
  Action ::
    { _actionName :: !Text
    , _actionUnderstandAs :: ![Text]
    , _actionParseArguments :: !(ActionParseArguments s v)
    , _actionBeforeRules :: !(ActionRulebook s v)
    , _actionCheckRules :: !(ActionRulebook s v)
    , _actionCarryOutRules :: !(ActionRulebook s v)
    , _actionReportRules :: !(ActionRulebook s v)
    } -> Action s

data Activity o v r = Activity
    { _activityName :: !Text
    , _activityDefault :: Maybe r
    , _activityBeforeRules :: !(Rulebook o v v ())
    , _activityCarryOutRules :: !(Rulebook o v v r)
    , _activityAfterRules :: !(Rulebook o v v ())
    }

data LocaleVariables s = LocaleVariables
  { _localePriorities :: LocalePriorities s
  , _localeDomain :: !(AnyObject s)
  , _localeParagraphCount :: Int
  }

-- | TODO
data ActivityCollection s = ActivityCollection
  { printingNameOfADarkRoom :: !(Activity s () ())
  , printingNameOfSomething :: !(Activity s (AnyObject s) ())
  , printingDescriptionOfADarkRoom :: !(Activity s () ())
  , choosingNotableLocaleObjects :: !(Activity s (AnyObject s) (LocalePriorities s))
  , printingLocaleParagraphAbout :: !(Activity s (LocaleInfo s) (LocaleVariables s))
  , describingLocale :: !(Activity s (LocaleVariables s) ())
  }

data World s = World
  { _title :: !Text
  , _entityCounter :: !(Entity, Entity)
  , _dirtyTime :: !Bool
  , _globalTime :: !Timestamp
  , _darknessWitnessed :: !Bool
  , _roomDescriptions :: !RoomDescriptions
  , _previousRoom :: !Entity
  , _currentPlayer :: !Entity
  , _firstRoom :: !(Maybe Entity)
  , _things :: !(Store (AbstractThing s))
  , _rooms :: !(Store (AbstractRoom s))
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  , _actions :: !(Map Text (Action s))
  , _activities :: !(ActivityCollection s)
  , _whenPlayBegins :: !(Rulebook s () () Bool)
  , _messageBuffers :: !(MessageBuffer, MessageBuffer)
  , _actionProcessing :: !(forall m. MonadWorld s m => Action s -> UnverifiedArgs s -> m (Maybe Bool))
  }

-- | An abstract interface for logging functions which are capable of reporting
-- source locations.
class Monad m => Logger m where
  debug :: HasCallStack => TLB.Builder -> m ()
  info :: HasCallStack => TLB.Builder -> m ()
  warn :: HasCallStack => TLB.Builder -> m ()
  err :: HasCallStack => TLB.Builder -> m ()
  withContext :: HasCallStack => TLB.Builder -> m a -> m a

newtype Game s a = Game
  { unGame :: KatipContextT (StateT (World s) IO) a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, Katip, KatipContext, MonadState (World s))

-- | All 'KatipLogger's fulfill the logging interface described by 'Logger'.
instance Logger (Game s) where
  debug = logItemM (toLoc ?callStack) DebugS . LogStr
  info = logItemM (toLoc ?callStack) InfoS . LogStr
  warn = logItemM (toLoc ?callStack) WarningS . LogStr
  err = logItemM (toLoc ?callStack) ErrorS . LogStr
  withContext n = katipAddNamespace (Namespace [toStrict $ TLB.toLazyText n])

instance Logger m => Logger (ExceptT e m) where
  debug = lift . debug
  info = lift . info
  warn = lift . warn
  err = lift . err
  withContext b (ExceptT f) = ExceptT (withContext b f)

instance Logger m => Logger (MaybeT m) where
  debug = lift . debug
  info = lift . info
  warn = lift . warn
  err = lift . err
  withContext b (MaybeT f) = MaybeT (withContext b f)

-- | Try to extract the last callsite from some GHC 'CallStack' and convert it
-- to a 'Loc' so that it can be logged with 'logItemM'.
toLoc :: CallStack -> Maybe Loc
toLoc stk =
  let mLoc = listToMaybe . reverse $ getCallStack stk
   in mLoc <&> \(_, loc) ->
        Loc
          { loc_filename = srcLocFile loc,
            loc_package = srcLocPackage loc,
            loc_module = srcLocModule loc,
            loc_start = (srcLocStartLine loc, srcLocStartCol loc),
            loc_end = (srcLocEndLine loc, srcLocEndCol loc)
          }

instance MonadReader (World s) (Game s) where
  ask = get
  local f g = do
    s <- get
    put (f s)
    r <- g
    put s
    return r

data MissingObject s = MissingObject Text Entity
--in case we have both a read-only and a read-write constraint on the world.
type MonadWorld s m = (MonadReader (World s) m, MonadState (World s) m, Logger m)
type MonadWorldRO s m = (MonadReader (World s) m, Logger m)
type NoMissingObjects s m = (MonadError (MissingObject s) m)

newtype YaiflItem a = YaiflItem
  { toKatipItem :: Item a
  } deriving newtype (Generic, Functor)

instance A.ToJSON a => A.ToJSON (YaiflItem a) where
    toJSON (YaiflItem Item{..}) = A.object $
      [ "level" A..= _itemSeverity
      , "message" A..= B.toLazyText (unLogStr _itemMessage)
      , "timestamp" A..= formatAsLogTime _itemTime
      , "ns" A..= let f = T.intercalate "➤" (filter (/= T.empty) $ unNamespace _itemNamespace) in if T.empty == f then "" else "❬"<>f<>"❭"
      , "loc" A..= fmap reshapeFilename _itemLoc
      ] ++ ["data" A..=  _itemPayload | A.encode _itemPayload /= "{}"]

reshapeFilename :: Loc -> String
reshapeFilename Loc{..} = drop 1 (dropWhile (/= '/') loc_filename) <> ":" <> show (fst loc_start) <> ":" <> show (snd loc_start)

makeLenses ''World
makeLenses ''Object
makeLenses ''Args
makeLenses ''Rulebook
makeLenses ''ThingData
makeLenses ''RoomData
makeLenses ''Enclosing
makeLenses ''TimestampedObject
makeLenses ''Container
makePrisms ''ObjectSpecifics
makeLenses ''LocaleVariables

--instance HasBuffer (World s) 'LogBuffer where
--  bufferL _ = messageBuffers % _2

instance HasBuffer (World s) 'SayBuffer where
  bufferL _ = messageBuffers % _1

instance Functor (Args s) where
  fmap f = argsVariables %~ f
instance Functor (Object s) where
  fmap f = objData %~ f
instance Foldable (Object s) where
  foldMap f = f . _objData
instance Traversable (Object s) where
  traverse f o = (\v -> o {_objData = v}) <$> f (_objData o)