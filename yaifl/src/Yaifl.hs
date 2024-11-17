{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE RecordWildCards #-}

module Yaifl (
  newWorld
  , blankWorld
  , HasStandardProperties
  , PlainWorldModel
  , ActivityCollection(..)
  , ActionCollection(..)
  , blankActionCollection
  , Game
  , runGame
  , addStandardActions
  , runTurnsFromBuffer
  , runTurn

  , module Yaifl.Model.Metadata
  , module Yaifl.Game.World
  , module Yaifl.Model.WorldModel
  ) where

import Yaifl.Prelude hiding ( Reader, runReader )

import Yaifl.Model.Action
import Yaifl.Game.ActionProcessing
import Yaifl.Game.Actions.Going
import Yaifl.Game.Actions.Looking
import Yaifl.Game.Actions.Looking.Locale
import Yaifl.Game.Actions.Looking.Visibility
import Yaifl.Game.Actions.SwitchingOn
import Yaifl.Model.Activity
import Yaifl.Game.Activities.ChoosingNotableLocaleObjects
import Yaifl.Game.Activities.ListingContents
import Yaifl.Game.Activities.PrintingLocaleParagraphAbout
import Yaifl.Game.Activities.PrintingTheLocaleDescription
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Direction
import Yaifl.Model.Entity
import Yaifl.Game.ObjectSpecifics
import Yaifl.Game.Create.Object
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Door
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.WorldModel
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Game.WhenPlayBegins
import Yaifl.Text.AdaptiveNarrative (blankAdaptiveNarrative)
import Yaifl.Text.Print
import Yaifl.Text.ResponseCollection
import Yaifl.Text.Say
import Yaifl.Game.World

import qualified Data.Map as DM
import qualified Data.Text as T
import Yaifl.Text.ListWriter
import Yaifl.Game.Actions.OutOfWorld
import Yaifl.Model.Actions.Args
import Yaifl.Game.EffectHandlers
import Yaifl.Text.DynamicText
import Yaifl.Game.Actions.Collection
import Breadcrumbs
import Yaifl.Model.Query (failHorriblyIfMissing)
import Yaifl.Game.Actions.Examining
import Yaifl.Model.Store
import Yaifl.Game.Actions.Closing
import Yaifl.Game.Actions.Opening
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Room
import Yaifl.Model.ObjectKind
import qualified Data.Map as M
import Yaifl.Model.Input (waitForInput)
import Yaifl.Game.Parser
import Yaifl.Game.Actions.Taking
import Yaifl.Model.Kinds.Device
import Yaifl.Game.Activities.PrintingRoomDescriptionDetails
import qualified Data.Set as S
import Yaifl.Game.TurnSequence (turnSequenceRules, everyTurnRules)
import Yaifl.Model.Rules.Run
import System.Random.Stateful
import Yaifl.Game.Actions.Entering (enteringAction)
import Yaifl.Game.Actions.Waiting
import Yaifl.Game.Actions.Exiting (exitingAction)

type PlainWorldModel = 'WorldModel ObjectSpecifics Direction () () ActivityCollection ResponseCollection DynamicText

type HasStandardProperties s = (
  WMWithProperty s Enclosing
  , WMWithProperty s Openability
  , WMWithProperty s Container
  , WMWithProperty s Enterable
  , WMWithProperty s Device
  , HasLookingProperties s
  , WMStdDirections s
  , WMWithProperty s Door
  , HasDirectionalTerms s
  , Pointed (WMObjSpecifics s)
  )

-- | All the standard library activities.
-- printing the banner text, constructing the status line, reading a command, deciding the scope
-- clarifying the parser's scope, printing a paser error, asking which do you mean
-- supplying a missing noun, second noun, implicitly taking, amusing a victorius player, printing obit
-- handling final question, offering something and performing something (both dialogue)
data ActivityCollection wm = ActivityCollection
  { choosingNotableLocaleObjects :: Activity wm () (AnyObject wm) (LocalePriorities wm)
  , groupingTogether :: Activity wm () (Thing wm) ()
  , listingContents :: Activity wm () (ListWritingParameters wm) ()
  , listingNondescriptItems :: Activity wm () (AnyObject wm) ()
  , printingANumberOf :: Activity wm () (Int, Thing wm) ()
  , printingDescriptionOfADarkRoom :: Activity wm () () ()
  , printingLocaleParagraphAbout :: Activity wm () (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
  , printingNameOfSomething :: Activity wm () (AnyObject wm) Text
  , printingTheLocaleDescription :: Activity wm YouCanAlsoSeeResponses (LocaleVariables wm) ()
  , printingNameOfADarkRoom :: Activity wm () () ()
  , printingRoomDescriptionDetails :: Activity wm () (Thing wm) ()
  , printingInventoryDetails :: Activity wm () (Thing wm) ()
  -- TODO https://ganelson.github.io/inform/standard_rules/S-act.html#SP15
{-

  , writingAParagraphAbout :: Activity wm (AnyObject wm) ()
  , printingAnnouncementOfDarkness :: Activity wm () ()
  , printingAnnouncementOfLight :: Activity wm () ()
  , printingRefusalToActInTheDark :: Activity wm () ()
  , decidingConcealedPossessions :: Activity wm () ()
  , decidingWhetherAllIncludes :: Activity wm () ()
-}
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActivityCollection

addStandardActions ::
  Breadcrumbs :> es
  => State (ActionCollection wm) :> es
  => State (WorldActions wm) :> es
  => Eff es ()
addStandardActions = do
  ActionCollection{..} <- get
  addAction going
  addAction looking
  addAction examining
  addAction opening
  addAction closing
  addAction switchingOn
  addAction taking
  addAction entering
  addAction waiting
  addAction exiting
  pass

blankActions ::
  HasStandardProperties wm
  => WorldActions wm
blankActions = WorldActions
  { actionsMap = DM.empty
  , whenPlayBegins = whenPlayBeginsRules
  , actionProcessing = actionProcessingRules
  , turnSequence = turnSequenceRules
  , everyTurn = everyTurnRules
  }

blankStores :: WorldStores s
blankStores = WorldStores
  { entityCounter = (Entity 1, Entity (-1))
  , things = emptyStore
  , rooms = emptyStore
  , values = DM.empty
  , concepts = ()
  , regions = emptyStore
  }

blankMetadata :: Metadata
blankMetadata = Metadata
  { title = "Untitled"
  , roomDescriptions = NoAbbreviatedRoomDescriptions
  , globalTime = 0
  , darknessWitnessed = False
  , currentPlayer = defaultPlayerID
  , currentStage = Construction
  , previousRoom = voidID
  , firstRoom = voidID
  , errorLog = []
  , kindDAG = M.map (\s  -> ObjectKindInfo s [] []) makeKindDAG
  , traceAnalysisLevel = Maximal
  , oxfordCommaEnabled = True
  , parserMatchThreshold = 0.66
  , bufferedInput = []
  , mentionedThings = S.empty
  , rng = mkStdGen 69
  }

newWorld ::
  Pointed (WMObjSpecifics wm)
  => HasLookingProperties wm
  => HasDirectionalTerms wm
  => WMStdDirections wm
  => Eff (EffStack wm) ()
newWorld = failHorriblyIfMissing $ do
  addBaseObjects
  addBaseActions

blankActivityCollection ::
  HasStandardProperties wm
  => ActivityCollection wm
blankActivityCollection = ActivityCollection
  { printingNameOfADarkRoom = blankActivity "printing the name of a dark room"
  , printingNameOfSomething = printingNameOfSomethingImpl
  , printingDescriptionOfADarkRoom = blankActivity "printing the description of a dark room"
  , choosingNotableLocaleObjects = choosingNotableLocaleObjectsImpl
  , printingLocaleParagraphAbout = printingLocaleParagraphAboutImpl
  , printingTheLocaleDescription = printingTheLocaleDescriptionImpl
  , listingNondescriptItems = blankActivity "listing nondescript items"
  , listingContents = listingContentsImpl
  , groupingTogether = blankActivity "grouping things together"
  , printingANumberOf = blankActivity "printing a number of"
  , printingInventoryDetails = blankActivity "printing inventory details of"
  , printingRoomDescriptionDetails = printingRoomDescriptionDetailsImpl
  }

blankActionCollection ::
  HasStandardProperties wm
  => ActionCollection wm
blankActionCollection = ActionCollection
  { going = goingAction
  , looking = lookingAction
  , examining = examiningAction
  , opening = openingAction
  , closing = closingAction
  , switchingOn = switchingOnAction
  , taking = takingAction
  , entering = enteringAction
  , exiting = exitingAction
  , waiting = waitingAction
  }

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

addGoingSynonyms ::
  forall wm es.
  State (WorldActions wm) :> es
  => HasDirectionalTerms wm
  => Bounded (WMDirection wm)
  => Enum (WMDirection wm)
  => Show (WMDirection wm)
  => Eff es ()
addGoingSynonyms = do
  forM_ (universe @(WMDirection wm)) $ \dir -> do
    let allTerms = toTextDir (Proxy @wm) dir
        dirN = (T.toLower . fromString . show) dir
    forM_ allTerms $ \term -> addInterpretAs term ("go " <> dirN) [NoParameter]
  addInterpretAs "out" "exit" [NoParameter]

addInterpretAs ::
  State (WorldActions wm) :> es
  => Text
  -> Text
  -> [NamedActionParameter wm]
  -> Eff es ()
addInterpretAs term interp params = actionsMapL % at term ?= Interpret (InterpretAs interp params)

addBaseActions ::
  WMStdDirections wm
  => Breadcrumbs :> es
  => HasDirectionalTerms wm
  => State (WorldActions wm) :> es
  => State (ActionCollection wm) :> es
  => Eff es ()
addBaseActions = do
  addStandardActions
  addGoingSynonyms
  addOutOfWorldActions


-- https://github.com/ganelson/inform/blob/06cfa98854af5289170e8565d0265b316a9f3745/inform7/extensions/standard_rules/Sections/Command%20Grammar.w#L225
addOutOfWorldActions ::
  forall wm es.
  State (WorldActions wm) :> es
  => Eff es ()
addOutOfWorldActions = do
  addOutOfWorld ["superbrief", "short"] superbriefAction
  addOutOfWorld ["verbose", "long"] verboseAction
  addOutOfWorld ["brief", "normal"] briefAction


addOutOfWorld ::
  forall wm es.
  State (WorldActions wm) :> es
  => [Text]
  -> OutOfWorldAction wm
  -> Eff es ()
addOutOfWorld cs e = forM_ cs $ \c ->
  #actionsMap % at c ?= OtherAction e

runTurnsFromBuffer ::
  RuleEffects wm es
  => State (WorldActions wm) :> es
  => Eff es ()
runTurnsFromBuffer = do
  b <- use @Metadata #bufferedInput
  unless (null b) $ runTurn >> runTurnsFromBuffer

runTurn ::
  forall wm es.
  State (WorldActions wm) :> es
  => RuleEffects wm es
  => Eff es ()
runTurn = do
  let actionOpts = ActionOptions False False
  wa <- get @(WorldActions wm)
  runRulebook Nothing False (wa ^. #turnSequence) ()
  printPrompt actionOpts
  i <- waitForInput
  printText i
  void $ parseAction actionOpts [NoParameter] i
  -- TODO: this is where every turn things happen
