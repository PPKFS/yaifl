module Yaifl.Types
  (
  -- * Objects
  Entity (..)
  , Timestamp(..)
  , ObjType(..)
  , Store(..)
  , StoreLens'
  , Object (..)
  , Thing
  , Room
  , AnyObject
  , AbstractObject (..)
  , TimestampedObject (..)
  , AbstractThing
  , AbstractRoom
  , AnyAbstractObject
  , ObjectUpdate
  -- * Object Data
  , ThingLit(..)
  , ThingData(..)

  , Darkness(..)
  , IsVisited(..)
  , RoomData(..)

    -- * Object Specifics
  , Enclosing(..)
  , Container(..)
  , Opacity(..)
  , Enterable(..)
  , Openable(..)
  , ObjectSpecifics(..)
  -- * Rules
  , Rulebook (..)
  , Rule (..)
  -- * Actions
  , ActionRulebook
  , Action (..)
  , UnverifiedArgs
  , ActivityCollection(..)
  , Args (..)
  -- * World
  , World (..)
  , MessageBuffer (..)
  , RoomDescriptions (..)
  -- * Functions
  , eqObject
  -- * Lenses
  , actions
  , whenPlayBegins
  , concepts
  , rbRules
  , firstRoom
  , objData
  , objSpecifics
  , thingContainedBy
  , roomEnclosing
  , roomDarkness
  , tsCachedObject
  , things
  , rooms
  , argsSource
  , globalTime
  , title
  , entityCounter

  , thingLit

  , enclosingContains
  , containerEnclosing
  , containerOpenable
  , containerEnterable

  , _EnclosingSpecifics
  , _ContainerSpecifics
  ) where

import Yaifl.Prelude
import Yaifl.Messages
import qualified Data.EnumSet as ES
import qualified Data.EnumMap.Strict as EM

-- | An 'Entity' is an integer ID that is used to reference between objects.
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic, Eq)
    deriving newtype (Num, Enum, Ord)

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

type StoreLens' s d = (Lens' (World s) (Store (AbstractObject s d)))

instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)

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
    deriving newtype (Num, Enum, Ord)

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

type AnyObject s = Object s (Either ThingData RoomData)
-- | Function to update an object
type ObjectUpdate s d = (Object s d -> World s -> Object s d)

type Thing s = Object s ThingData
type Room s = Object s RoomData
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
  , _runRule :: v -> World s -> ((v, Maybe r), World s)
  }

-- | A 'Rulebook' is a composition of functions ('Rule's) with short-circuiting (if
-- a Just value is returned), default return values, and a way to parse
-- 'UnverifiedArgs' into a @v@.
data Rulebook o ia v r where
  Rulebook ::
    { _rbName :: !Text
    , _rbDefaultOutcome :: !(Maybe r)
    , _rbParseArguments :: !(ParseArguments o ia v)
    , _rbRules :: ![Rule o v r]
    } -> Rulebook o ia v r

-- | Arguments for an action, activity, or rulebook. These are parameterised over
-- the closed 'Property' universe and the variables, which are either unknown
-- (see 'UnverifiedArgs') or known (concrete instantation).
data Args s v = Args
  { _argsSource :: !(Maybe (AnyObject s))
  , _argsVariables :: !v
  , _argsTimestamp :: !Timestamp
  }

-- | Before 'Args' are parsed, the variables are a list of objects.
type UnverifiedArgs s = Args s [AnyObject s]

-- | 'ActionRulebook's have specific variables
type ActionRulebook o v = Rulebook o (Args o v) (Args o v) Bool
type StandardRulebook o v r = Rulebook o (UnverifiedArgs o) v r
type ParseArguments o ia v = ia -> World o -> Maybe v
type ActionParseArguments o v = UnverifiedArgs o -> World o -> Maybe v

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action o where
  Action ::
    { _actionName :: !Text
    , _actionUnderstandAs :: ![Text]
    , _actionParseArguments :: !(ActionParseArguments o v)
    , _actionBeforeRules :: !(ActionRulebook o v)
    , _actionCheckRules :: !(ActionRulebook o v)
    , _actionCarryOutRules :: !(ActionRulebook o v)
    , _actionReportRules :: !(ActionRulebook o v)
    } -> Action o

-- | TODO
data ActivityCollection o = ActivityCollection
  { _dummy1 :: !Int
  , _dummy2 :: !Int
  }

data World o = World
  { _title :: !Text
  , _entityCounter :: !(Entity, Entity)
  , _globalTime :: !Timestamp
  , _darknessWitnessed :: !Bool
  , _roomDescriptions :: !RoomDescriptions
  , _currentPlayer :: !Entity

  , _firstRoom :: !(Maybe Entity)
  , _things :: !(Store (AbstractThing o))
  , _rooms :: !(Store (AbstractRoom o))
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  , _actions :: !(Map Text (Action o))
  , _activities :: !(ActivityCollection o)
  , _whenPlayBegins :: !(Rulebook o () () Bool)
  , _messageBuffers :: !(MessageBuffer, MessageBuffer)
  , _actionProcessing :: !(Action o -> UnverifiedArgs o ->
                           World o -> (Maybe Bool, World o))
  }

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

instance HasBuffer (World s) 'LogBuffer where
  bufferL _ = messageBuffers % _2

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