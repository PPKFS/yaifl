{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl (
  newWorld
  , blankWorld
  , HasStandardProperties
  , PlainWorldModel
  , ActivityCollection(..)
  , Text'(..)
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
import Yaifl.Core.Metadata
import Yaifl.Core.Object
import Yaifl.Core.Objects.Create
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Enclosing
import Yaifl.Core.Properties.Has
import Yaifl.Core.Rulebooks.ActionProcessing
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Rulebooks.WhenPlayBegins
import Yaifl.Core.World
import Yaifl.Core.WorldModel
import Yaifl.Lamp.Actions.Going
import Yaifl.Lamp.Actions.Looking
import Yaifl.Lamp.Activities.ChoosingNotableLocaleObjects
import Yaifl.Lamp.Activities.DescribingLocale
import Yaifl.Lamp.Activities.PrintingDescriptionOfADarkRoom as Activity
import Yaifl.Lamp.Activities.PrintingLocaleParagraphAbout
import Yaifl.Lamp.ObjectSpecifics
import Yaifl.Lamp.Properties.Container
import Yaifl.Lamp.Properties.Door
import Yaifl.Lamp.Properties.Openable
import Yaifl.Lamp.Visibility
import qualified Data.Map as DM
import qualified Data.Text as T
import Breadcrumbs
import Text.Interpolation.Nyan
import Yaifl.Core.Print
import Yaifl.Lamp.Responses
import Data.Text.Display
import Data.Text.Lazy.Builder (fromText)
import Yaifl.Core.AdaptiveNarrative (AdaptiveNarrative, blankAdaptiveNarrative)
import Yaifl.Lamp.Say
import Effectful.Writer.Static.Local

newtype Text' (wm :: WorldModel) =  Text' (Either Text (Text, RuleLimitedEffect wm (Writer Text) ()))

instance Display (Text' wm) where
  displayBuilder (Text' (Left t)) = fromText t
  displayBuilder (Text' (Right (n, _))) = fromText n

instance IsString (Text' wm) where
  fromString = Text' . Left . toText

instance SayableValue (Text' wm) wm where
  sayTell (Text' (Left t)) = tell t
  sayTell (Text' (Right (_, RuleLimitedEffect e))) = inject e


type PlainWorldModel = 'WorldModel ObjectSpecifics Direction () () ActivityCollection ResponseCollection Text'

type HasStandardProperties s = (
  WMHasProperty s Enclosing
  , WMHasProperty s Container
  , WMHasProperty s Enterable
  , WMHasProperty s Openable
  , HasLookingProperties s
  , WMStdDirections s
  , WMHasProperty s Door
  , HasDirectionalTerms s)

blankWorld ::
  HasStandardProperties wm
  => (ActivityCollection wm -> ActivityCollector wm)
  -> (ResponseCollection wm -> ResponseCollector wm)
  -> World (wm :: WorldModel)
blankWorld mkAcColl mkRsColl = World
  { metadata = blankMetadata
  , stores = blankStores
  , actions = blankActions
  , messageBuffer = blankMessageBuffer
  , activities = mkAcColl blankActivityCollection
  , responses = mkRsColl blankResponseCollection
  , adaptiveNarrative = blankAdaptiveNarrative
  }

blankActions :: HasProperty (WMObjSpecifics s) Enclosing => WorldActions s
blankActions = WorldActions
  { actions = DM.empty
  , whenPlayBegins = whenPlayBeginsRules
  , actionProcessing = actionProcessingRules
  }

blankActivityCollection ::
  HasStandardProperties wm
  => ActivityCollection wm
blankActivityCollection = ActivityCollection
  { printingNameOfADarkRoom = printingDescriptionOfADarkRoomImpl
  , printingNameOfSomething = printingNameOfSomethingImpl
  , printingDescriptionOfADarkRoom = printingDescriptionOfADarkRoomImpl
  , choosingNotableLocaleObjects = choosingNotableLocaleObjectsImpl
  , printingLocaleParagraphAbout = printingLocaleParagraphAboutImpl
  , describingLocale = describingLocaleImpl
  }

blankStores :: WorldStores s
blankStores = WorldStores
  { entityCounter = (Entity 1, Entity (-1))
  , things = emptyStore
  , rooms = emptyStore
  , values = DM.empty
  , concepts = ()
  }

blankMetadata :: Metadata
blankMetadata = Metadata
  { title = "Untitled"
  , roomDescriptions = SometimesAbbreviatedRoomDescriptions
  , globalTime = 0
  , darknessWitnessed = False
  , currentPlayer = Entity 1
  , currentStage = Construction
  , previousRoom = voidID
  , firstRoom = voidID
  , errorLog = []
  , typeDAG = makeTypeDAG
  , traceAnalysisLevel = Maximal
  }

type EffStack wm = '[
  ActionHandler wm
  , State (AdaptiveNarrative wm)
  , State (ResponseCollector wm)
  , State (ActivityCollector wm)
  , ObjectTraverse wm
  , ObjectUpdate wm
  , ObjectLookup wm
  , Reader [Text]
  , State Metadata
  , State (WorldActions wm)
  , ObjectCreation wm
  , Print
  , State (World wm)
  , Breadcrumbs
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
  forall wm a.
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm, Display (WMSayable wm), SayableValue (WMSayable wm) wm)
  => TraceID
  -> World wm
  -> Eff (EffStack wm) a
  -> IO (a, World wm)
convertToUnderlyingStack tId w =
  runEff
  . runBreadcrumbs (Just tId)
  . runStateShared w
  . runPrintPure @(World wm)
  . runCreationAsLookup
  . zoomState #actions
  . zoomState @(World wm) #metadata
  . runReader []
  . runQueryAsLookup
  . runTraverseAsLookup
  . zoomState @(World wm) #activities
  . zoomState @(World wm) #responses
  . zoomState @(World wm) #adaptiveNarrative
  . runActionHandlerAsWorldActions

-- TODO: there's probably a much nicer way to make these traverse lens nicely
runTraverseAsLookup ::
  State (World wm) :> es
  => ObjectUpdate wm :> es
  => Eff (ObjectTraverse wm : es) a
  -> Eff es a
runTraverseAsLookup = interpret $ \env -> \case
  TraverseThings f -> do
    m <- use $ #stores % #things
    mapM_ (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r setThing) m
  TraverseRooms f -> do
    m <- use $ #stores % #rooms
    mapM_ (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r setRoom) m

runCreationAsLookup ::
  State (World wm) :> es
  => Eff (ObjectCreation wm : es) a
  -> Eff es a
runCreationAsLookup = interpret $ \_ -> \case
  GenerateEntity bThing -> if bThing then
    (#stores % #entityCounter % _1) <<%= (+1) else (#stores % #entityCounter % _2) <<%= (\x -> x-1)
  AddRoom aRoom -> #stores % #rooms % at (getID aRoom) ?= aRoom
  AddThing aThing -> #stores % #things % at (getID aThing) ?= aThing

runQueryAsLookup ::
  HasCallStack
  => State (World wm) :> es
  => Eff (ObjectUpdate wm : ObjectLookup wm : es) a
  -> Eff es a
runQueryAsLookup = interpretLookup  . interpretUpdate

interpretLookup ::
  forall wm es a.
  HasCallStack
  => State (World wm) :> es
  => Eff (ObjectLookup wm : es) a
  -> Eff es a
interpretLookup = do
  let lookupHelper ::
        Entity
        -> Lens' (WorldStores wm) (Store (Object wm d))
        -> Lens' (WorldStores wm) (Store (Object wm e))
        -> Text
        -> Text
        -> Eff es (Either Text (Object wm d))
      lookupHelper e l l' expected errTy = do
            let i = getID e
            mbObj <- use $ #stores % l % at i
            case mbObj of
              Nothing -> do
                mbRoom <- use $ #stores % l' % at i
                let (cs :: Text) = show callStack
                case mbRoom of
                  Nothing -> pure $ Left [int|t|Could not find the object #s{i} as either a thing or room (Queried as a #{expected}).|]
                  Just _ -> pure $ Left [int|t|Tried to lookup a #{errTy} as a #{expected}: #{i}. (at: #{cs}) |]
              Just ao -> pure $ Right ao
  interpret $ \_ -> \case
    LookupThing e -> lookupHelper (getID e) #things #rooms "thing" "room"
    LookupRoom e -> lookupHelper (getID e) #rooms #things "room" "thing"

interpretUpdate ::
  State (World wm) :> es
  => Eff (ObjectUpdate wm : es) a
  -> Eff es a
interpretUpdate = interpret $ \_ -> \case
  SetRoom r -> #stores % #rooms % at (getID r) %= updateIt r
  SetThing t -> #stores % #things % at (getID t) %= updateIt t

updateIt :: Object wm d -> Maybe (Object wm d) -> Maybe (Object wm d)
updateIt newObj mbExisting = case mbExisting of
  Nothing -> Just newObj
  Just _ -> Just newObj

runGame ::
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm, Display (WMSayable wm), SayableValue (WMSayable wm) wm)
  => TraceID
  -> World wm
  -> Eff (EffStack wm) a
  -> IO (a, World wm)
runGame = convertToUnderlyingStack

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
    #actions % at dirN ?= Left (InterpretAs ("go " <> dirN))

makeTypeDAG :: Map ObjectType (Set ObjectType)
makeTypeDAG = fromList
  [ ("object", fromList [])
  , ("thing", fromList ["object"])
  , ("room", fromList ["object"])
  , ("container", fromList ["thing"])
  , ("supporter", fromList ["thing"])
  ]

data ActivityCollection wm = ActivityCollection
  { printingNameOfADarkRoom :: !(Activity wm () ())
  , printingNameOfSomething :: !(Activity wm (AnyObject wm) ())
  , printingDescriptionOfADarkRoom :: !(Activity wm () ())
  , choosingNotableLocaleObjects :: !(Activity wm (AnyObject wm) (LocalePriorities wm))
  , printingLocaleParagraphAbout :: !(Activity wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm))
  , describingLocale :: !(Activity wm (LocaleVariables wm) ())
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActivityCollection