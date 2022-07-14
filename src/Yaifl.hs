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
import Yaifl.Core.Rulebooks.ActionProcessing
import Cleff.Trace (runTraceStderr)
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Actions.Parser
import Yaifl.Lamp.Actions.Looking
import Prelude hiding (int, Reader, runReader)
import Yaifl.Core.Actions.Activity
import Yaifl.Lamp.Activities.PrintingNameOfADarkRoom as Activity
import Yaifl.Lamp.Activities.PrintingNameOfSomething as Activity
import Yaifl.Lamp.Activities.PrintingDescriptionOfADarkRoom as Activity
import Yaifl.Lamp.Activities.ChoosingNotableLocaleObjects
import Yaifl.Lamp.Activities.PrintingLocaleParagraphAbout
import Yaifl.Lamp.Activities.DescribingLocale
import Text.Interpolation.Nyan
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

blankWorld :: HasStandardProperties wm => World (wm :: WorldModel)
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

blankActivityCollection ::
  HasStandardProperties wm
  => ActivityCollection wm
blankActivityCollection = ActivityCollection
  { printingNameOfADarkRoom = printingNameOfADarkRoomImpl
  , printingNameOfSomething = printingNameOfSomethingImpl
  , printingDescriptionOfADarkRoom = printingDescriptionOfADarkRoomImpl
  , choosingNotableLocaleObjects = choosingNotableLocaleObjectsImpl
  , printingLocaleParagraphAbout = printingLocaleParagraphAboutImpl
  , describingLocale = describingLocaleImpl
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
  , _traceAnalysisLevel = Maximal
  }

type family (xs :: [(Type -> Type) -> Type -> Type]) :++: (ys :: [(Type -> Type) -> Type -> Type]) :: [(Type -> Type) -> Type -> Type] where
  '[]       :++: ys = ys
  (x ': xs) :++: ys = x ': (xs :++: ys)

type EffStack wm = EffStackNoIO wm :++: '[IOE]
type EffStackNoIO wm = '[
  ActionHandler
  , State (ActivityCollection wm)
  , ObjectTraverse wm  
  , ObjectUpdate wm
  , ObjectLookup wm
  , Log
  , Reader [Text]
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
  . zoom worldActions
  . zoom worldMetadata
  . runReader []
  . runTraceStderr
  . runLoggingAsTrace
  . runQueryAsLookup
  . runTraverseAsLookup
  . zoom worldActivities
  . runActionHandlerAsWorldActions
  . raiseUnderN @(State (World wm)) @(EffStackNoIO wm) @'[IOE]

-- TODO: there's probably a much nicer way to make these traverse lens nicely
runTraverseAsLookup ::
  '[State (World wm), ObjectUpdate wm, ObjectCreation wm, State (Metadata wm)] :>> es
  => Eff (ObjectTraverse wm : es)
  ~> Eff es
runTraverseAsLookup = interpret \case
  TraverseThings f -> do
    m <- use $ worldStores % things
    mapM_ (\aT -> do
      r <- reifyThing aT >>= (toEff . f)
      whenJust r setThing) m
  TraverseRooms f -> do
    m <- use $ worldStores % rooms
    mapM_ (\aT -> do
      r <- reifyRoom aT >>= (toEff . f)
      whenJust r setRoom) m

runCreationAsLookup ::
  State (World wm) :> es
  => Eff (ObjectCreation wm : es)
  ~> Eff es
runCreationAsLookup = interpret \case
  GenerateEntity bThing -> if bThing then
    (worldStores % entityCounter % _1) <<%= (+1) else (worldStores % entityCounter % _2) <<%= (\x -> x-1)
  AddAbstractRoom aRoom -> worldStores % rooms % at (getID aRoom) ?= aRoom
  AddAbstractThing aThing -> worldStores % things % at (getID aThing) ?= aThing

runQueryAsLookup ::
  HasCallStack
  => Log :> es
  => State (World wm) :> es
  => (ObjectCreation wm :> es)
  => (State (Metadata wm) :> es)
  => Eff (ObjectUpdate wm : ObjectLookup wm : es)
  ~> Eff es
runQueryAsLookup = interpretLookup  . interpretUpdate

interpretLookup ::
  forall wm es. 
  HasCallStack
  => Log :> es
  => State (World wm) :> es
  => (ObjectCreation wm :> es)
  => (State (Metadata wm) :> es)
  => Eff (ObjectLookup wm : es)
  ~> Eff es
interpretLookup = do
  let lookupHelper :: 
        Entity 
        -> Lens' (WorldStores wm) (Store (AbstractObject wm d))
        -> (AbstractObject wm d -> Eff (NoMissingObject : es) (Object wm d))
        -> Lens' (WorldStores wm) (Store (AbstractObject wm e))
        -> Text
        -> Text
        -> Eff es (Either Text (Object wm d))
      lookupHelper e l reify l' expected errTy = do
            let i = getID e
            mbObj <- use $ worldStores % l % at i
            case mbObj of
              Nothing -> do
                mbRoom <- use $ worldStores % l' % at i
                let (cs :: Text) = show callStack
                noteError Left $ case mbRoom of
                  Nothing -> [int|t|Could not find the object #s{i} as either a thing or room (Queried as a #{expected}).|]
                  Just a -> [int|t|Tried to lookup a #{errTy} as a #{expected}: #{i}. (at: #{cs}) |] :: Text
              Just ao -> 
                withoutMissingObjects (Right <$> reify ao)
                  (\mo -> noteError Left [int|t|Failed to reify #{mo}.|])
  interpret \case
    LookupThing e -> lookupHelper (getID e) things reifyThing rooms "thing" "room"
    LookupRoom e -> lookupHelper (getID e) rooms reifyRoom things "room" "thing"

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
  HasStandardProperties wm
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
