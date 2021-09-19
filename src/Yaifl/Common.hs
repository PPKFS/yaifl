{-|
Module      : Yaifl.Common
Description : Mostly defining types to be used everywhere and some helper functions.
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
  , IsSubtype(..)
    -- ** Objects
  , Object (..)
  , Thing
  , Room
  , Concept
  , ThingData(..)
  , RoomData(..)
  , ConceptData(..)
  , ObjType(..)
  , ObjectUpdate

  , ThingProperties (..)
  , RoomProperties (..)
  , ConceptProperties (..)

  , AbstractObject (..)
  , TimestampedObject (..)
  , AbstractRoom
  , AbstractThing
  , AbstractConcept
    -- ** Rules and Actions
  , Rulebook (..)
  , Rule (..)

  , Action (..)
  , UnverifiedArgs
  , Args (..)

  -- * Smart constructors
  , emptyStore
  , defaultActivities
  , blankThingData
  , blankRoomData

  -- * Lenses
  , actions
  , whenPlayBegins
  , things
  , rooms
  , concepts
  , rbRules

  -- * World lookups and modifications
  , getGlobalTime
  , setTitle
  , newEntityID
  )
where

import Control.Lens
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Relude
import Yaifl.Messages

class IsSubtype sup sub where
  inject :: sub -> sup

instance IsSubtype a a where
  inject = id

-- | An 'Entity' is an integer ID that is used to reference between objects.
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic)
    deriving newtype (Num, Enum)

-- | A 'Store' is a map from 'Entity's to @a@s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  }

-- first let's define our own alterF for EnumMap...
alterEMF
  :: (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

-- | alterF wrapper for Store, since it's a wrapper around a wrapper...
alterSF
  :: Functor f
  => (Maybe a -> f (Maybe a))
  -> Entity
  -> Store a -> f (Store a)
alterSF upd k m = Store <$> alterEMF upd k (unStore m)

instance At (Store a) where
  at k f = alterSF f k

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a) where
  ix k f m = case EM.lookup k (unStore m) of
     Just v -> Store <$> (f v <&> \v' -> EM.insert k v' (unStore m))
     Nothing -> pure m
  {-# INLINE ix #-}

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

-- | Whether a room has been visited before or not.
data IsVisited = Visited | Unvisited deriving (Eq, Show)

-- | The connections from a given room to another
type MapConnections = Store Entity

-- | An abstract grouping of rooms.
type ContainingRegion = Maybe Entity

-- | An 'Object' is any kind of game object. The important part is @a@; which should
-- probably be one of three kinds (thus parameterisation by t r c):
-- 'ThingData t'
-- 'RoomData r'
-- 'ConceptData c'
data Object a = Object
  { _objName :: !Text
  , _objDescription :: !Text
  , _objID :: !Entity
  , _objType :: !ObjType
  , _objCreationTime :: !Timestamp
  , _objDetails :: !a
  }

-- | Details for room objects. This is anything which is...well, a room. Nontangible.
data RoomData a = RoomData
  { _roomIsVisited :: !IsVisited
  , _roomDarkness :: !Darkness
  , _roomMapConnections :: !MapConnections
  , _roomContainingRegion :: !ContainingRegion
  , _roomDetails :: !a
  }

-- | Details for things. This is anything tangible.
data ThingData a = ThingData
  { _thingDetails :: !a
  , _thingDummy :: ()
  }

-- | Details for concepts. These are intangible, predominantly knowledge facts (for
-- instance, the knowledge of an actor about the location of an item). This is where
-- yaifl differs from Inform.
data ConceptData a = ConceptData
  { _conceptDetails :: !a
  , _conceptDummy :: ()
  }

-- | A 'TimestampedObject' is an object which has been cached at time '_tsCacheStamp'
-- and contains a function to update it given the state of the world. For instance,
-- this allows descriptions to be dynamic.
data TimestampedObject t r c o = TimestampedObject
  { _tsCachedObject :: !(Object o)
  , _tsCacheStamp :: !Timestamp
  , _tsUpdateFunc :: ObjectUpdate t r c o
  }

-- | Function to update an object
type ObjectUpdate t r c o = Object o -> World t r c -> Object o

-- | An abstract object is either a static object (which does not need to update itself)
-- or a timestamped object. Whilst this is what is stored internally, you shouldn't
-- need to pass these around; instead reify the object with 'reifyObject'.
data AbstractObject t r c o
  = DynamicObject (TimestampedObject t r c o)
  | StaticObject (Object o)

-- | A property is the closed universe of all possible object types. This is to
-- ensure we don't have to worry about existentials.
data Property t r c
  = ThingProperty (ThingData t)
  | RoomProperty (RoomData r)
  | ConceptProperty (ConceptData c)

-- | These 3 are the standard universes for each of the object subtypes. To extend
-- these, define an injection that allows for access of these as a subtype of a larger
-- property type.
data ThingProperties = PlainObject
data RoomProperties = PlainRoom
data ConceptProperties = PlainConcept

type Thing t = Object (ThingData t)
type Room r = Object (RoomData r)
type Concept c = Object (ConceptData c)

type AbstractThing t r c = AbstractObject t r c (ThingData t)
type AbstractRoom t r c = AbstractObject t r c (RoomData r)
type AbstractConcept t r c = AbstractObject t r c (ConceptData c)

blankThingData :: IsSubtype u ThingProperties => ThingData u
blankThingData = ThingData (inject PlainObject) ()

blankRoomData :: IsSubtype r RoomProperties => RoomData r
blankRoomData = RoomData Unvisited Lighted emptyStore Nothing (inject PlainRoom)

-- | Arguments for an action, activity, or rulebook. These are parameterised over
-- the closed 'Property' universe and the variables, which are either unknown
-- (see 'UnverifiedArgs') or known (concrete instantation).
data Args t ro c v = Args
  { _argsSource :: !(Maybe (Object (Property t ro c)))
  , _argsVariables :: !v
  , _argsTimestamp :: !Timestamp
  }

-- | Before 'Args' are parsed, the variables are a list of objects.
type UnverifiedArgs t ro c = Args t ro c [Object (Property t ro c)]

type RuleOutcome = Bool

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule t ro c v r = Rule
  { _ruleName :: !Text
  , _runRule :: v -> World t ro c -> ((v, Maybe r), World t ro c)
  }

-- | A 'Rulebook' is a composition of functions ('Rule's) with short-circuiting (if
-- a Just value is returned), default return values, and a way to parse
-- 'UnverifiedArgs' into a @v@.
data Rulebook t ro c v r where
  Rulebook ::
    { _rbName :: !Text
    , _rbDefaultOutcome :: !(Maybe r)
    , _rbParseArguments :: !(ParseArguments t ro c v)
    , _rbRules :: ![Rule t ro c v r]
    } -> Rulebook t ro c v r

-- | 'ActionRulebook's have specific variables
type ActionRulebook t ro c v = Rulebook t ro c (Args t ro c v) RuleOutcome

type ParseArguments t ro c v =  UnverifiedArgs t ro c -> World t ro c -> Either Text v

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action t ro c where
  Action ::
    { _actionName :: !Text
    , _actionUnderstandAs :: ![Text]
    , _actionParseArguments :: !(ParseArguments t ro c (Args t ro c v))
    , _actionBeforeRules :: !(ActionRulebook t ro c v)
    , _actionCheckRules :: !(ActionRulebook t ro c v)
    , _actionCarryOutRules :: !(ActionRulebook t ro c v)
    , _actionReportRules :: !(ActionRulebook t ro c v)
    } -> Action t ro c

-- | Again lifted directly from Inform; this sets whether to always print room
-- descriptions (No..) even if the room is visited, to only print them on the first
-- entry (Sometimes..) or never.
data RoomDescriptions
  = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions
  deriving (Eq, Show)

-- | TODO
data ActivityCollection t r c = ActivityCollection
  { _dummy1 :: !Int
  , _dummy2 :: !Int
  }

-- | TODO
defaultActivities :: ActivityCollection t r c
defaultActivities = ActivityCollection 1 1

-- | TODO: split this into 3 - stores, config, bookkeeping?
data World t r c = World
  { _title :: !Text
  , _entityCounter :: !Entity
  , _globalTime :: !Timestamp
  , _darknessWitnessed :: !Bool
  , _roomDescriptions :: !RoomDescriptions

  , _firstRoom :: !(Maybe Entity)
  , _things :: !(Store (AbstractThing t r c))
  , _rooms :: !(Store (AbstractRoom t r c))
  , _concepts :: !(Store (AbstractConcept t r c))
  , _actions :: !(Store (Action t r c))
  , _activities :: !(ActivityCollection t r c)
  , _whenPlayBegins :: !(Rulebook t r c () Text)
  , _messageBuffers :: !(MessageBuffer, MessageBuffer)
  , _actionProcessing :: !(Action t r c -> UnverifiedArgs t r c ->
                           World t r c -> (Maybe Text, World t r c))
  }

makeLenses ''World
makeLenses ''Object
makeLenses ''Args
makeLenses ''Rulebook

instance HasBuffer (World t r c) 'LogBuffer where
  bufferL _ = messageBuffers . _2

instance HasBuffer (World t r c) 'SayBuffer where
  bufferL _ = messageBuffers . _1

-- | Obtain the current timestamp. This is a function in case I want to change the
-- implementation in the future.
getGlobalTime
  :: World t r c
  -> Timestamp
getGlobalTime = _globalTime

-- | Update the game title.
setTitle
  :: Text -- ^ New title.
  -> World u r c
  -> World u r c
setTitle = (title .~)

-- | Generate a new entity ID.
newEntityID
  :: World u r c
  -> (Entity, World u r c)
newEntityID = entityCounter <<+~ 1

reifyObject :: AbstractObject t r c o -> World t r c -> (Object o, World t r c)
reifyObject = error ""