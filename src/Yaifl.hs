module Yaifl (
  newWorld
  , blankWorld
  , HasStandardProperties
  , PlainWorldModel
  , Game
  , runGame
  ) where

import Solitude hiding ( Reader, runReader )
import Effectful.Dispatch.Dynamic ( interpret, localSeqUnlift )
import Effectful.Optics ( (?=), (%=), use, (<<%=) )
import Effectful.Reader.Static ( runReader, Reader )

import Yaifl.Core.Actions.Action
import Yaifl.Core.Actions.Activity
import Yaifl.Core.Actions.Parser
import Yaifl.Core.Direction
import Yaifl.Core.Entity
import Yaifl.Core.Logger
import Yaifl.Core.Metadata
import Yaifl.Core.Object
import Yaifl.Core.Objects.Create
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Enclosing
import Yaifl.Core.Properties.Has
import Yaifl.Core.Rulebooks.ActionProcessing
import Yaifl.Core.Rulebooks.Rule (ActionHandler)
import Yaifl.Core.Rulebooks.WhenPlayBegins
import Yaifl.Core.Say
import Yaifl.Core.World
import Yaifl.Core.WorldModel
import Yaifl.Lamp.Actions.Going
import Yaifl.Lamp.Actions.Looking
import Yaifl.Lamp.Activities.ChoosingNotableLocaleObjects
import Yaifl.Lamp.Activities.DescribingLocale
import Yaifl.Lamp.Activities.PrintingDescriptionOfADarkRoom as Activity
import Yaifl.Lamp.Activities.PrintingLocaleParagraphAbout
import Yaifl.Lamp.Activities.PrintingNameOfADarkRoom as Activity
import Yaifl.Lamp.Activities.PrintingNameOfSomething as Activity
import Yaifl.Lamp.ObjectSpecifics
import Yaifl.Lamp.Properties.Container
import Yaifl.Lamp.Properties.Door
import Yaifl.Lamp.Properties.Openable
import Yaifl.Lamp.Visibility
import qualified Data.Map as DM
import qualified Data.Text as T

type PlainWorldModel = 'WorldModel ObjectSpecifics Direction () ()

type HasStandardProperties s = (
  WMHasProperty s Enclosing
  , WMHasProperty s Container
  , WMHasProperty s Enterable
  , WMHasProperty s Openable
  , HasLookingProperties s
  , WMStdDirections s
  , WMHasProperty s Door
  , HasDirectionalTerms s)

blankWorld :: HasStandardProperties wm => World (wm :: WorldModel)
blankWorld = World
  { _worldMetadata = blankMetadata
  , _worldStores = blankStores
  , _worldActions = blankActions
  , _messageBuffer = blankMessageBuffer
  , _worldLogs = LB []
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

blankMetadata :: Metadata
blankMetadata = Metadata
  { _title = "Untitled"
  , _roomDescriptions = SometimesAbbreviatedRoomDescriptions
  , _dirtyTime = False
  , _globalTime = 0
  , _darknessWitnessed = False
  , _currentPlayer = Entity 1
  , _currentStage = Construction
  , _previousRoom = voidID
  , _firstRoom = voidID
  , _errorLog = []
  , _typeDAG = makeTypeDAG
  , _traceAnalysisLevel = Maximal
  }

type EffStack wm = '[
  ActionHandler wm
  , State (ActivityCollection wm)
  , ObjectTraverse wm
  , ObjectUpdate wm
  , ObjectLookup wm
  , Log
  , Reader [Text]
  , State Metadata
  , State (WorldActions wm)
  , ObjectCreation wm
  , Saying
  , State (World wm)
  , IOE
  ]

type Game wm = Eff (EffStack wm)

type UnderlyingEffStack wm = '[State (World wm), IOE]

newWorld ::
  HasLookingProperties wm

  => WMStdDirections wm
  => WMHasProperty wm Door
  => Eff (EffStack wm) ()
newWorld = do
  addBaseObjects
  addBaseActions

zoomState ::
  (State whole :> es)
  => Lens' whole sub
  -> (Eff (State sub ': es)) a
  -> Eff es a
zoomState l = interpret $ \env -> \case
  Get      -> gets (view l)
  Put s    -> modify (set l s)
  State f  -> state (\s -> second (\x -> s & l .~ x) $ f (s ^. l))
  StateM f -> localSeqUnlift env $ \unlift -> stateM
    (\s -> do
      newSub <- unlift $ f (s ^. l)
      pure $ second (\x -> s & l .~ x) newSub )

convertToUnderlyingStack ::
  forall wm.
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => World wm
  -> Eff (EffStack wm) ()
  -> IO ((), World wm)
convertToUnderlyingStack w =
  runEff
  . runStateShared w
  . runSayPure @(World wm)
  . runCreationAsLookup
  . zoomState worldActions
  . zoomState worldMetadata
  . runReader []
  . runLoggingInternal @(World wm)
  . runQueryAsLookup
  . runTraverseAsLookup
  . zoomState worldActivities
  . runActionHandlerAsWorldActions

-- TODO: there's probably a much nicer way to make these traverse lens nicely
runTraverseAsLookup ::
  '[State (World wm), ObjectUpdate wm, ObjectCreation wm, State Metadata] :>> es
  => Eff (ObjectTraverse wm : es) a
  -> Eff es a
runTraverseAsLookup = interpret $ \env -> \case
  TraverseThings f -> do
    m <- use $ worldStores % things
    mapM_ (\aT -> do
      r <- reifyThing aT >>= (\r -> localSeqUnlift env $ \unlift -> unlift $ f r)
      whenJust r setThing) m
  TraverseRooms f -> do
    m <- use $ worldStores % rooms
    mapM_ (\aT -> do
      r <- reifyRoom aT >>= (\r -> localSeqUnlift env $ \unlift -> unlift $ f r)
      whenJust r setRoom) m

runCreationAsLookup ::
  State (World wm) :> es
  => Eff (ObjectCreation wm : es) a
  -> Eff es a
runCreationAsLookup = interpret $ \_ -> \case
  GenerateEntity bThing -> if bThing then
    (worldStores % entityCounter % _1) <<%= (+1) else (worldStores % entityCounter % _2) <<%= (\x -> x-1)
  AddAbstractRoom aRoom -> worldStores % rooms % at (getID aRoom) ?= aRoom
  AddAbstractThing aThing -> worldStores % things % at (getID aThing) ?= aThing

runQueryAsLookup ::
  HasCallStack
  => Log :> es
  => State (World wm) :> es
  => (ObjectCreation wm :> es)
  => (State Metadata :> es)
  => Eff (ObjectUpdate wm : ObjectLookup wm : es) a
  -> Eff es a
runQueryAsLookup = interpretLookup  . interpretUpdate

interpretLookup ::
  forall wm es a.
  HasCallStack
  => Log :> es
  => State (World wm) :> es
  => (ObjectCreation wm :> es)
  => (State Metadata :> es)
  => Eff (ObjectLookup wm : es) a
  -> Eff es a
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
                  Just _ -> [int|t|Tried to lookup a #{errTy} as a #{expected}: #{i}. (at: #{cs}) |] :: Text
              Just ao ->
                withoutMissingObjects (Right <$> reify ao)
                  (\mo -> noteError Left [int|t|Failed to reify #s{mo}.|])
  interpret $ \_ -> \case
    LookupThing e -> lookupHelper (getID e) things reifyThing rooms "thing" "room"
    LookupRoom e -> lookupHelper (getID e) rooms reifyRoom things "room" "thing"

interpretUpdate ::
  State (World wm) :> es
  => (State Metadata :> es)
  => Eff (ObjectUpdate wm : es) a
  -> Eff es a
interpretUpdate = interpret $ \_ -> \case
  SetRoom r -> getGlobalTime >>= \ts -> worldStores % rooms % at (getID r) %= updateIt ts r
  SetThing t -> getGlobalTime >>= \ts -> worldStores % things % at (getID t) %= updateIt ts t

updateIt :: Timestamp -> Object wm d -> Maybe (AbstractObject wm d) -> Maybe (AbstractObject wm d)
updateIt ts newObj mbExisting = case mbExisting of
  Nothing -> Just (StaticObject newObj)
  Just (StaticObject _) -> Just (StaticObject newObj)
  Just (DynamicObject (TimestampedObject _ _ f)) -> Just (DynamicObject (TimestampedObject newObj ts f))

runGame ::
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => World wm
  -> Text
  -> Eff (EffStack wm) ()
  -> IO (World wm)
runGame wo _ f = do
  (_, w) <- convertToUnderlyingStack wo f
  return w

addBaseActions ::
  (HasLookingProperties wm)
  => WMStdDirections wm
  => WMHasProperty wm Door
  => State (WorldActions wm) :> es
  => Eff es ()
addBaseActions = do
  addAction lookingAction
  addAction goingAction
  addGoingSynonyms

addGoingSynonyms ::
  forall wm es.
  (State (WorldActions wm) :> es, Bounded (WMDirection wm), Enum (WMDirection wm), Show (WMDirection wm))
  => Eff es ()
addGoingSynonyms = forM_ (universe @(WMDirection wm)) $ \dir ->
    let dirN = (T.toLower . fromString . show) dir in
    actions % at dirN ?= Left (InterpretAs ("go " <> dirN))

makeTypeDAG :: Map ObjType (Set ObjType)
makeTypeDAG = fromList
  [ ("object", fromList [])
  , ("thing", fromList ["object"])
  , ("room", fromList ["object"])
  , ("container", fromList ["thing"])
  , ("supporter", fromList ["thing"])
  ]