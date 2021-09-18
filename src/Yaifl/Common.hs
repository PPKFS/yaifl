module Yaifl.Common (
    World (..)
  , ThingProperties (..)
  , RoomProperties (..)
  , ConceptProperties (..)
  , Rulebook(..)
  , Rule(..)
  , Name
  , Description
  , Entity(..)
  , Action(..)
  , MessageBuffer(..)
  , RoomDescriptions(..)
  , UnverifiedArgs
  , Args(..)
  , Object(..)
  , Thing
  , Timestamp
  , Store
  , AbstractObject(..)
  , TimestampedObject(..)

  , emptyStore

  , actions
  , getGlobalTime
  , whenPlayBegins
  , messageBuffer

  , defaultActivities
  , source
  , objects

  , rules

  , buffer

  , setTitle
  , addThing
  , addThing'
  , addRoom
  , addRoom'
  , makeRule
  , ruleEnd
  
  , setStyle
  , say
  , sayLn
  , sayIf) where

import Relude

import Control.Lens
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

-- | An 'Entity' is an integer ID that is used to reference between objects.
newtype Entity = Entity {unID :: Int} deriving newtype Num

type Store a = IM.IntMap a

emptyStore :: Store a
emptyStore = IM.empty
class IsWorld w

type Name = Text
type Description = Text
type Timestamp = Int

newtype ObjType = ObjType {unObjType :: Text} deriving (Show)

data Darkness = Lighted | Dark deriving (Eq, Show)
data IsVisited = Visited | Unvisited deriving (Eq, Show)
newtype MapConnections = MapConnections {unMapConnections :: Store Entity}
type ContainingRegion = Maybe Entity

defaultActivities = error ""
data Object a = Object
    { _name :: Name
    , _description :: Description
    , _objID :: Entity
    , _objType :: ObjType
    , _creationTime :: Timestamp
    , _details :: a
    }

data RoomData a = RoomData
    { __isVisited :: IsVisited
    , __darkness :: Darkness
    , __mapConnections :: MapConnections
    , _containingRegion :: ContainingRegion
    , _roomSpecifics :: a
    }

data ThingData a = ThingData
    {  _thingSpecifics :: a
    }

data ConceptData a = ConceptData
    {  _conceptSpecifics :: a
    }

makeLenses ''Object


data TimestampedObject t r c o = TimestampedObject
    { _cachedObject :: Object o
    , _cacheStamp :: Timestamp
    , _updateFunc :: ObjectUpdate t r c o
    }

type ObjectUpdate t r c o = World t r c -> Object o -> Object o
{-
So the hierarchy of stuff is as follows.
everything is an Object. The collection of all kinds of Object is a Universe.
The Universe consists of a ThingUniverse and a RoomUniverse.
There are standard defined universes but these can be extended by defining InUniverse instances to inject the smaller standard universe type
into an extended universe type.
A thing has physical info (out of laziness that there aren't many non-physical things...so I guess I'll just make an isTangible flag and
ignore stuff that falls as intangible)
an AbstractObject is either a static object (that does not update itself, but may be updated by other processes)
or a DynamicObject, which is a wrapper around a timestamped object.
a timestamped object is a cached reified object at a certain timestamp, plus a function that can update e.g. descriptions if it is deemed out of date
so in this sense an abstract object is the potential for an object. if you want to look stuff up, you need to reify it first. 
if you're modifying it, you call modifyObject that just modifies the cached object?
-}

data AbstractObject t r c o = DynamicObject (TimestampedObject t r c o) | StaticObject (Object o)

data Property t r c = ThingProp (ThingData t) | RoomProp (RoomData r) | ConceptProp (ConceptData c)
data ThingProperties = PlainObject
data RoomProperties = PlainRoom
data ConceptProperties = PlainConcept

type Thing t = Object (ThingData t)
type Room r = Object (RoomData r)
type Concept c = Object (ConceptData c)

type AbstractRoom t r c = AbstractObject t r c (RoomData r)
type AbstractThing t r c = AbstractObject t r c (ThingData t)
type AbstractConcept t r c = AbstractObject t r c (ConceptData c)
class IsSubtype sup sub where
    inject :: sub -> sup

instance IsSubtype a a where
    inject = id

type StyledDoc = PP.Doc PPTTY.AnsiStyle
data MessageBuffer = MessageBuffer
    { _buffer :: [StyledDoc]
    , _msgStyle :: Maybe PPTTY.AnsiStyle
    }
--I think this means I need some kind of injection
--but what I'm doing for laziness is u
--the only sane instantiation for u is Property t r c so
data Args t ro c v = Args
    { _source :: Maybe (Object (Property t ro c))
    ,  actionVariables :: v
    , _timestamp :: Timestamp
    }

type UnverifiedArgs t ro c = Args t ro c [Object (Property t ro c)]
type RuleOutcome = Bool
data Rule t ro c v r = Rule
    { _ruleName :: Text
    , _runRule ::  v -> World t ro c ->((v, Maybe r), World t ro c)
    }
data Rulebook t ro c v r where
    Rulebook :: { _rulebookName :: Text
    , _defaultOutcome :: Maybe r
    , _parseRulebookArguments :: UnverifiedArgs t ro c -> World t ro c -> Either Text v
    , _rules :: [Rule t ro c v r]
    } -> Rulebook t ro c v r


type ActionRulebook t ro c v = Rulebook t ro c (Args t ro c v) RuleOutcome
data Action t ro c where
    Action :: { _actionName :: Text
    , _understandAs :: [Text]
    , _parseArguments :: UnverifiedArgs t ro c -> World t ro c -> Either Text (Args t ro c v)
    , _beforeActionRules :: ActionRulebook t ro c v
    , _checkActionRules :: ActionRulebook t ro c v
    , _carryOutActionRules :: ActionRulebook t ro c v
    , _reportActionRules :: ActionRulebook t ro c v
    } -> Action t ro c
    {-
data Activity t ro c where
    Activity :: { _activityName :: Text
    , _parseActivityArguments :: UnverifiedArgs t ro c -> Either Text (Args t ro c v)
    , _beforeRules :: ActionRulebook t ro c v
    , _forRules :: ActionRulebook t ro c v
    , _afterRules :: ActionRulebook t ro c v
    } -> Activity t ro c
-}
data RoomDescriptions = SometimesAbbreviatedRoomDescriptions | AbbreviatedRoomDescriptions | NoAbbreviatedRoomDescriptions deriving (Eq, Show)

data ActivityCollection t r c = ActivityCollection
    { _d :: Int
    , _e :: Int
    }

data World t r c = World
    { _title :: Text
    , _firstRoom :: Maybe Entity
    , _entityCounter :: Entity
    , _darknessWitnessed :: Bool
    , _roomDescriptions :: RoomDescriptions
    , _objects :: Store (AbstractThing t r c)
    , _rooms :: Store (AbstractRoom t r c)
    , _concepts :: Store (AbstractConcept t r c)
    , _messageBuffer :: MessageBuffer
    , _globalTime :: Timestamp
    , _actions :: Store (Action t r c)
    , _activities :: ActivityCollection t r c
    , _whenPlayBegins :: Rulebook t r c () Text
    , _actionProcessing :: Action t r c -> UnverifiedArgs t r c -> World t r c -> (Maybe Text, World t r c)
    }
makeLenses ''World
makeLenses ''MessageBuffer
makeLenses ''Args
makeLenses ''Rulebook

getGlobalTime :: World t r c -> Timestamp
getGlobalTime = _globalTime

-- I wanted to add something like 'concepts' as an intangible object kind, "knowing it's there" vs "almost certain" vs "maybe" etc
-- | this is a collection of all the possible 'a's
setTitle :: Text -> World u r c -> World u r c
setTitle = (title .~)

newEntityID :: World u r c-> (Entity, World u r c)
newEntityID w = w & entityCounter <<+~ 1

-- | create a new object (room or thing), but don't add it to any stores or anything.
makeObject :: Name -> Description -> ObjType -> o -> Maybe (ObjectUpdate u r c o) -> World u r c -> (AbstractObject u r c o, World u r c)
makeObject n d ty specifics upd = runState (do
    e <- state newEntityID
    t <- gets _globalTime
    let obj = Object n d e ty t specifics
    return $ case upd of
        Nothing -> StaticObject obj
        Just upd' -> DynamicObject $ TimestampedObject obj t upd')

addInternal :: (AbstractObject u r c o -> World u r c -> World u r c) -> Name -> Description -> ObjType -> o -> Maybe (ObjectUpdate u r c o) -> World u r c -> World u r c
addInternal updWorld n d ty specifics updateFunc = execState (do
    o <- state $ makeObject n d ty specifics updateFunc
    modify $ updWorld o)

addThing :: IsSubtype u ThingProperties => Name -> Description -> ObjType -> Maybe (ThingData u) -> Maybe (ObjectUpdate u r c (ThingData u)) -> World u r c-> World u r c
addThing n d ot spec = addInternal updateObjects n d ot (fromMaybe blankThingData spec)

addRoom :: IsSubtype r RoomProperties => Name -> Description -> ObjType -> Maybe (RoomData r) -> Maybe (ObjectUpdate u r c (RoomData r)) -> World u r c -> World u r c
addRoom n d ot spec = addInternal updateRooms n d ot (fromMaybe blankRoomData spec)

addThing' :: IsSubtype u ThingProperties => Name -> Description -> State (ThingData u) v -> World u r c -> World u r c
addThing' n d stateUpdate = addThing n d (ObjType "thing") (Just (execState stateUpdate blankThingData)) Nothing

addRoom' :: IsSubtype r RoomProperties => Name -> Description -> State (RoomData r) v -> World u r c -> World u r c
addRoom' n d rd = addRoom n d (ObjType "room") (Just (execState rd blankRoomData)) Nothing

makeRule :: Text -> (World t ro c -> (Maybe r, World t ro c)) -> Rule t ro c () r
makeRule n f = Rule n (\_ w -> first ((), ) $ f w)

ruleEnd :: World t0 r0 c0 -> (Maybe r, World t0 r0 c0)
ruleEnd = (Nothing ,)

blankThingData :: IsSubtype u ThingProperties => ThingData u
blankThingData = ThingData (inject PlainObject)

blankRoomData :: IsSubtype r RoomProperties => RoomData r
blankRoomData = RoomData Unvisited Lighted (MapConnections emptyStore) Nothing (inject PlainRoom)

updateInternal :: Lens' (World u r c) (Store (AbstractObject u r c o)) -> AbstractObject u r c o -> World u r c -> World u r c
updateInternal l o w =  w & l . at (unID $ getID' o) ?~ o where
    getID' (StaticObject o') = _objID o'
    getID' (DynamicObject t) = (_objID . _cachedObject) t

updateRooms :: AbstractRoom t r c -> World t r c -> World t r c
updateRooms = updateInternal rooms

updateObjects :: AbstractThing t r c -> World t r c -> World t r c
updateObjects = updateInternal objects

sayInternal :: StyledDoc -> World t r c -> World t r c
sayInternal a w = w & messageBuffer . buffer %~ (:) (maybe id PP.annotate f a)
    where f = w ^. messageBuffer . msgStyle

say :: Text -> World t r c -> World t r c
say = sayInternal . PP.pretty

sayLn :: Text -> World t r c -> World t r c
sayLn a = say (a <> "\n")

sayIf :: Bool -> Text -> World t r c -> World t r c
sayIf True a = say a
sayIf False _ = id

setStyle :: Maybe PPTTY.AnsiStyle -> World t r c -> World t r c
setStyle sty = messageBuffer . msgStyle .~ sty

--look :: ??? -> World u r -> World u r

--addRoom' :: IsSubtype r RoomProperties => Name -> Description -> Maybe (RoomData r) ->  World u r -> World u r
--addRoom' n d rd = addRoom n d rd "room"