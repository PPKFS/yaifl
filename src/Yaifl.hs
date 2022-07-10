-- ~\~ language=Haskell filename=src/Yaifl.hs
-- ~\~ begin <<lit/construction.md|src/Yaifl.hs>>[0] project://lit/construction.md:6

module Yaifl
  (
  newWorld
  , blankWorld
  , HasStandardProperties
  , PlainWorldModel
  , Game
  , runGame
  ) where

import Yaifl.Core.World
import qualified Data.Map as DM
import Yaifl.Core.Common
import Yaifl.Core.Say
--import Yaifl.Core.Rulebooks.ActionProcessing
import Yaifl.Core.Properties.Property
import Yaifl.Lamp.Properties.Openable
import Yaifl.Lamp.Properties.Container
import Yaifl.Core.Properties.Enclosing
import Yaifl.Core.Directions
import Yaifl.Core.Logger
import Cleff.State hiding ( zoom )
import Cleff.Reader
import Yaifl.Core.Objects.Create
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.WhenPlayBegins
import Yaifl.Core.Actions.Action
import Display
import Yaifl.Core.Rulebooks.ActionProcessing
import Cleff.Trace (runTraceStderr)
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Actions.Parser
import Yaifl.Lamp.Actions.Looking
import Prelude hiding (Reader, runReader)
import Yaifl.Core.Actions.Activity
import qualified Yaifl.Lamp.Activities.PrintingNameOfADarkRoom as Activity
import qualified Yaifl.Lamp.Activities.PrintingNameOfSomething as Activity
import qualified Yaifl.Lamp.Activities.PrintingDescriptionOfADarkRoom as Activity
--import Yaifl.Core.Objects.Create
--import Yaifl.Core.Rulebooks.WhenPlayBegins
--import Yaifl.Core.ActivityCollection
--import Yaifl.Core.Directions

type PlainWorldModel = 'WorldModel () Direction () ()

type HasStandardProperties s = (
  WMHasProperty s Enclosing
  , WMHasProperty s Container
  , WMHasProperty s Enterable
  , WMHasProperty s Openable)

blankWorld :: HasProperty (WMObjSpecifics s) Enclosing => World (s :: WorldModel)
blankWorld = World
  { _worldMetadata = blankMetadata
  , _worldStores = blankStores
  , _worldActions = blankActions
  , _messageBuffer = blankMessageBuffer
  , _worldActivities = blankActivityCollection
  }

blankActions :: HasProperty (WMObjSpecifics s) Enclosing => WorldActions s
blankActions = WorldActions
  { _actions = DM.empty
  , _whenPlayBegins = whenPlayBeginsRules
  , _actionProcessing = actionProcessingRules
  }

blankActivityCollection :: ActivityCollection wm
blankActivityCollection = ActivityCollection
  { printingNameOfADarkRoom = printingNameOfADarkRoomImpl
  , printingNameOfSomething = printingNameOfSomethingImpl
  , printingDescriptionOfADarkRoom = printingDescriptionOfADarkRoomImpl
  , choosingNotableLocaleObjects = error ""
  , printingLocaleParagraphAbout = error ""
  , describingLocale = error ""
  }

blankStores :: WorldStores s
blankStores = WorldStores
  { _entityCounter = (Entity 1, Entity (-1))
  , _things = emptyStore
  , _rooms = emptyStore
  , _values = DM.empty
  , _concepts = ()
  }

blankMetadata :: Metadata s
blankMetadata = Metadata 
  { _title = "Untitled"
  , _roomDescriptions = SometimesAbbreviatedRoomDescriptions
  , _dirtyTime = False
  , _globalTime = 0
  , _darknessWitnessed = False
  , _currentPlayer = Entity 1
  , _currentStage = Construction
  , _previousRoom = defaultVoidID
  , _firstRoom = defaultVoidID
  , _errorLog = []
  , _typeDAG = makeTypeDAG
  }

type family (xs :: [(* -> *) -> * -> *]) :++: (ys :: [(* -> *) -> * -> *]) :: [(* -> *) -> * -> *] where
  '[]       :++: ys = ys
  (x ': xs) :++: ys = x ': (xs :++: ys)

type EffStack wm = EffStackNoIO wm :++: '[IOE]
type EffStackNoIO wm = '[
  ActionHandler
  , State (ActivityCollection wm)
  , Log
  , Reader [Text]
  , ObjectUpdate wm
  , ObjectLookup wm
  , State (Metadata wm)
  , State (WorldActions wm)
  , ObjectCreation wm
  , Saying
  ]

type Game wm = Eff (EffStack wm) 

type UnderlyingEffStack wm = '[State (World wm), IOE] 

newWorld :: 
  HasLookingProperties wm
  => Eff (EffStack wm) ()
newWorld = do
  addBaseObjects
  addBaseActions

convertToUnderlyingStack :: 
  forall wm. Eff (EffStack wm)
  ~> Eff (UnderlyingEffStack wm) 
convertToUnderlyingStack = 
  runSayPure
  . runCreationAsLookup
  . (zoom worldActions)
  . (zoom worldMetadata)
  . runQueryAsLookup
  . runReader []
  . runTraceStderr
  . runLoggingAsTrace
  . (zoom worldActivities)
  . runActionHandlerAsWorldActions
  . raiseUnderN @(State (World wm)) @(EffStackNoIO wm) @('[IOE])

runCreationAsLookup :: 
  State (World wm) :> es
  => Eff (ObjectCreation wm : es) 
  ~> Eff es
runCreationAsLookup = interpret \case
  GenerateEntity bThing -> if bThing then 
    ((worldStores % entityCounter % _1) <<%= (+1)) else ((worldStores % entityCounter % _2) <<%= (\x -> x-1))
  AddAbstractRoom aRoom -> worldStores % rooms % at (getID aRoom) ?= aRoom
  AddAbstractThing aThing -> worldStores % things % at (getID aThing) ?= aThing

runQueryAsLookup :: 
  State (World wm) :> es
  => (ObjectCreation wm :> es)
  => (State (Metadata wm) :> es)
  => Eff (ObjectUpdate wm : ObjectLookup wm : es) 
  ~> Eff es
runQueryAsLookup = interpretLookup  . interpretUpdate

interpretLookup ::
  State (World wm) :> es
  => (ObjectCreation wm :> es)
  => (State (Metadata wm) :> es)
  => Eff (ObjectLookup wm : es) 
  ~> Eff es
interpretLookup = interpret \case
  LookupThing e -> do
    mbObj <- use $ worldStores % things % at (getID e)
    case mbObj of
      Nothing -> return 
        if isRoom e 
          then 
            Left $ "Tried to lookup a room as a thing " <> displayText (getID e) 
          else 
            Left $ "Could not find " <> displayText (getID e)
      Just ao -> withoutMissingObjects (Right <$> reifyThing ao) (\mo -> return $ Left $ "Failed to reify " <> displayText mo)
  LookupRoom e -> do
    mbObj <- use $ worldStores % rooms % at (getID e)
    case mbObj of
      Nothing -> return 
        if isThing e 
          then 
            Left $ "Tried to lookup a thing as a room " <> displayText (getID e) 
          else 
            Left $ "Could not find " <> displayText (getID e)
      Just ao -> withoutMissingObjects (Right <$> reifyRoom ao) (\mo -> return $ Left $ "Failed to reify " <> displayText mo)

interpretUpdate ::
  State (World wm) :> es
  => (State (Metadata wm) :> es)
  => Eff (ObjectUpdate wm : es)
  ~> Eff es
interpretUpdate = interpret \case
  SetRoom r -> getGlobalTime >>= \ts -> worldStores % rooms % at (getID r) %= updateIt ts r
  SetThing t -> getGlobalTime >>= \ts -> worldStores % things % at (getID t) %= updateIt ts t

updateIt :: Timestamp -> Object wm d -> Maybe (AbstractObject wm d) -> Maybe (AbstractObject wm d)
updateIt ts newObj mbExisting = case mbExisting of
  Nothing -> Just (StaticObject newObj)
  Just (StaticObject _) -> Just (StaticObject newObj)
  Just (DynamicObject (TimestampedObject _ _ f)) -> Just (DynamicObject (TimestampedObject newObj ts f))

runGame :: 
  HasProperty (WMObjSpecifics wm) Enclosing 
  => Text 
  -> Eff (EffStack wm) a
  -> IO (World wm)
runGame _ f = do
  (_, w) <- runIOE $ runState blankWorld $ convertToUnderlyingStack f
  return w

addBaseActions :: 
  HasLookingProperties wm
  => State (WorldActions wm) :> es
  => Eff es ()
addBaseActions = do
  addAction lookingAction
  --addAction goingActionImpl

makeTypeDAG :: Map Text (Set Text)
makeTypeDAG = fromList 
  [ ("object", fromList [])
  , ("thing", fromList ["object"])
  , ("room", fromList ["object"])
  , ("container", fromList ["thing"])
  , ("supporter", fromList ["thing"])  
  ]

-- ~\~ end
