{-|
Module      : Yaifl.Common
Description : Mostly defining types to be used everywhere ans dome helper functions.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}
module Yaifl.Common
  ( -- * Types
    World (..)
  , Store
  , Entity (..)
  , MessageBuffer (..)
  , RoomDescriptions (..)
  , Timestamp
    -- ** Objects
  , Object (..)
  , ThingData(..)
  , RoomData(..)
  , ConceptData(..)
  , ObjType(..)
  , ObjectUpdate
  , HasID(..)

  , Enclosing(..)
  , Container(..)
  , Opacity(..)
  , Enterable(..)
  , Openable(..)
  , ThingLit(..)
  , Darkness(..)

  , Thing
  , Room

  , ThingProperties (..)
  , RoomProperties (..)
  , ConceptProperties (..)

  , ObjectSpecifics(..)

  , AbstractObject (..)
  , AnyObject
  , TimestampedObject (..)
  , AbstractThing
  , AbstractRoom
  , AnyAbstractObject

  , StoreLens'
    -- ** Rules and Actions
  , Rulebook (..)
  , RuleOutcome
  , Rule (..)
  , ActionRulebook

  , Action (..)
  , UnverifiedArgs
  , Args (..)

  -- * Smart constructors
  , emptyStore
  , defaultActivities
  , blankThingData
  , blankRoomData
  , defaultPlayerID
  , isThing
  , isRoom

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
  , objectL
  , tsCachedObject
  , things
  , rooms
  , argsSource

  , thingLit

  , conceptDetails

  , enclosingContains
  , containerEnclosing
  , containerOpenable
  , containerEnterable

  , _EnclosingSpecifics
  , _ContainerSpecifics

  -- * World lookups and modifications
  , getGlobalTime
  , tickGlobalTime
  , setTitle
  , newEntityID

  , reifyObject

  , isType
  )
where

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Yaifl.Prelude
import Yaifl.Messages


-- | An 'Entity' is an integer ID that is used to reference between objects.
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic, Eq)
    deriving newtype (Num, Enum, Ord)

newtype ThingID = ThingID { unThingID :: Int } deriving newtype (Num, Enum)
newtype RoomID = RoomID { unRoomID :: Int } deriving newtype (Num, Enum)

-- | A 'Store' is a map from 'Entity's to @a@s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  }  deriving stock (Show)

instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)

emptyStore :: Store a
emptyStore = Store EM.empty

-- | For now, a timestamp is simply an integer. The timestamp is updated whenever some
-- modification is made to the 'World'; therefore it does not directly correspond to
-- some sort of in-game turn counter. For example, throwing an object would result in
-- multiple timestamp jumps (an object moving, potential interactions on it hitting
-- something) whereas a sequence of 10 look actions will not (as the world does not
-- change). This is primarily used to ensure we can cache updates of objects that
-- change properties (e.g. strings).
type Timestamp = Int

-- | ObjTypes make a DAG that approximates inheritance; for instance, we may only care
-- that an object *is* a kind of food, but we don't necessarily know what the @a@ is
-- or looks like.
newtype ObjType = ObjType
  { unObjType :: Text }
  deriving (Show)

-- | Whether a room has an intrinsic light-ness. This isn't equivalent to whether a
-- room is currently dark - for instance, a cave may have light (if the player has a
-- lantern) but the cave will be Dark.
data Darkness = Lighted | Dark deriving (Eq, Show)

-- | Whether a thing is inherently lit or not. This counts for lighting up spaces.
data ThingLit = Lit | NotLit deriving (Eq, Show)

-- | Whether a room has been visited before or not.
data IsVisited = Visited | Unvisited deriving (Eq, Show)

-- | The connections from a given room to another
type MapConnections = Store Entity

-- | An abstract grouping of rooms.
type ContainingRegion = Maybe Entity

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container deriving stock (Show)

-- | An 'Object' is any kind of game object. The important part is @a@; which should
-- probably be one of three kinds (thus parameterisation by t r c):
-- 'ThingData t'
-- 'RoomData r'
-- 'ConceptData c'
data Object b a = Object
  { _objName :: !Text
  , _objDescription :: !Text
  , _objID :: !Entity
  , _objType :: !ObjType
  , _objCreationTime :: !Timestamp
  , _objSpecifics :: !(Either ObjectSpecifics b)
  , _objData :: !a
  } deriving stock (Generic, Show)

data ObjectData
-- | Details for room objects. This is anything which is...well, a room. Nontangible.
data RoomData = RoomData
  { _roomIsVisited :: !IsVisited
  , _roomDarkness :: !Darkness
  , _roomMapConnections :: !MapConnections
  , _roomContainingRegion :: !ContainingRegion
  , _roomEnclosing :: !Enclosing
  } deriving stock (Generic, Show)

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
  }
  deriving stock (Eq, Show)

makeLenses ''Container

emptyEnclosing :: Enclosing
emptyEnclosing = Enclosing ES.empty Nothing

-- | Details for concepts. These are intangible, predominantly knowledge facts (for
-- instance, the knowledge of an actor about the location of an item). This is where
-- yaifl differs from Inform.
data ConceptData = ConceptData
  { _conceptDetails :: !ConceptProperties
  , _conceptDummy :: ()
  }

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

-- | These 3 are the standard universes for each of the object subtypes. To extend
-- these, define an injection that allows for access of these as a subtype of a larger
-- property type.
data ThingProperties = PlainObject
data RoomProperties = PlainRoom
data ConceptProperties = PlainConcept

blankThingData :: ThingData
blankThingData = ThingData (Entity defaultVoidID) NotLit

blankRoomData
  :: RoomData
blankRoomData = RoomData Unvisited Lighted emptyStore Nothing emptyEnclosing

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

type RuleOutcome = Bool

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

-- | 'ActionRulebook's have specific variables
type ActionRulebook o v = Rulebook o (Args o v) (Args o v) RuleOutcome
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

-- | Again lifted directly from Inform; this sets whether to always print room
-- descriptions (No..) even if the room is visited, to only print them on the first
-- entry (Sometimes..) or never.
data RoomDescriptions
  = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions
  deriving (Eq, Show)

-- | TODO
data ActivityCollection o = ActivityCollection
  { _dummy1 :: !Int
  , _dummy2 :: !Int
  }

-- | TODO
defaultActivities :: ActivityCollection o
defaultActivities = ActivityCollection 1 1

defaultPlayerID :: Int
defaultPlayerID = 1

defaultVoidID :: Int
defaultVoidID = -1

-- | TODO: split this into 3 - stores, config, bookkeeping?
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
makeLenses ''ConceptData
makeLenses ''Enclosing
makeLenses ''TimestampedObject
makePrisms ''ObjectSpecifics

-- I can inject a smaller value into a larger value
-- so as long as I have a smaller value originally?
updateCachedObject
  :: TimestampedObject s d
  -> Object s d
  -> TimestampedObject s d
updateCachedObject ts o = ts & tsCachedObject .~ o


objectL :: Lens' (AbstractObject s d) (Object s d)
objectL = lens
  (\case
    StaticObject o -> o
    DynamicObject (TimestampedObject o _ _) -> o)
  (\o n -> case o of
    StaticObject _ -> StaticObject n
    DynamicObject ts -> DynamicObject $ updateCachedObject ts n
  )

isThing
  :: (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom
  :: (HasID a)
  => a
  -> Bool
isRoom = not . isThing

type StoreLens' s d = (Lens' (World s) (Store (AbstractObject s d)))

reifyObject
  :: StoreLens' s d
  -> AbstractObject s d
  -> World s
  -> (Object s d, World s)
reifyObject _ (StaticObject v) w = (v, w)
reifyObject l (DynamicObject t) w = if _tsCacheStamp t == getGlobalTime w
                    then (co, w)
                    else runState (do
                      -- update the object
                      updatedObj <- gets $ runUpdateFunction t co
                      -- update the world
                      l % at (getID co) ?= DynamicObject
                        (updateCachedObject t updatedObj)
                      return updatedObj) w
                    where co = _tsCachedObject t

runUpdateFunction
  :: TimestampedObject s d
  -> Object s d
  -> World s
  -> Object s d
runUpdateFunction =  _tsUpdateFunc
instance HasBuffer (World s) 'LogBuffer where
  bufferL _ = messageBuffers % _2

instance HasBuffer (World s) 'SayBuffer where
  bufferL _ = messageBuffers % _1

class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id

instance HasID (Object s d) where
  getID = _objID

instance HasID (AbstractObject s d) where
  getID (StaticObject o) = getID o
  getID (DynamicObject ts) = getID ts

instance HasID (TimestampedObject s d) where
  getID (TimestampedObject o _ _) = getID o

instance Functor (Args s) where
  fmap f = argsVariables %~ f
instance Functor (Object s) where
  fmap f = objData %~ f

instance Foldable (Object s) where
  foldMap f = f . _objData

instance Traversable (Object s) where
  traverse f o = (\v -> o {_objData = v}) <$> f (_objData o)


-- | object s d -> anyobject s (aka object s (either roomdata thingdata))
-- the function is object d -> world s -> object d
-- do I need some kind of d -> either a b? and then an either a b -> maybe d?
-- | Obtain the current timestamp. This is a function in case I want to change the
-- implementation in the future.
getGlobalTime
  :: World o
  -> Timestamp
getGlobalTime = _globalTime

tickGlobalTime
  :: State (World o) ()
tickGlobalTime = do
  globalTime %= (+1)
  logVerbose "Dong."

-- | Update the game title.
setTitle
  :: Text -- ^ New title.
  -> State (World o) ()
setTitle = (title .=)

-- | Generate a new entity ID.
newEntityID
  :: Bool
  -> World o
  -> (Entity, World o)
newEntityID True = entityCounter % _1 <<+~ 1
newEntityID False = entityCounter % _2 <<-~ 1

-- | Calculate whether one object type is a subclass of another
isType
  :: Object s d
  -> ObjType
  -> World s
  -> Bool
isType o t w = False

