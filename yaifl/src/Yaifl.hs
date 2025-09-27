{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE RecordWildCards #-}

module Yaifl (
  newWorld
  , blankWorld
  , HasStandardProperties
  , PlainWorldModel
  , ActivityCollection(..)
  , ActionCollection(..)
  , YaiflEffects
  , Game
  , runGame
  , addStandardActions
  , runTurnsFromBuffer
  , runTurn

  , module Yaifl.Core.Metadata
  , module Yaifl.Std.World
  , module Yaifl.Core.WorldModel
  ) where

import Yaifl.Prelude hiding ( Reader, runReader )

import Breadcrumbs
import System.Random.Stateful
import Yaifl.Core.Action
import Yaifl.Core.Actions.Args
import Yaifl.Core.Actions.GoesWith
import Yaifl.Core.Activity
import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Metadata
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.Rules.Run
import Yaifl.Core.Store
import Yaifl.Core.WorldModel
import Yaifl.Std.Rulebooks.Accessibility
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Std.Activities.ChoosingNotableLocaleObjects
import Yaifl.Std.Activities.ListingContents
import Yaifl.Std.Activities.PrintingLocaleParagraphAbout
import Yaifl.Std.Activities.PrintingRoomDescriptionDetails
import Yaifl.Std.Activities.PrintingTheLocaleDescription
import Yaifl.Std.EffectHandlers
import Yaifl.Std.ObjectSpecifics
import Yaifl.Std.Parser
import Yaifl.Std.Rulebooks.TurnSequence (turnSequenceRules, everyTurnRulesImpl)
import Yaifl.Std.Rulebooks.WhenPlayBegins
import Yaifl.Std.World
import Yaifl.Std.Actions.Collection
import Yaifl.Std.Actions.Looking.Locale
import Yaifl.Std.Actions.Looking.Visibility
import Yaifl.Std.Actions.OutOfWorld
import Yaifl.Std.Kinds.Direction
import Yaifl.Std.Kinds.ObjectKind
import Yaifl.Text.AdaptiveNarrative (blankAdaptiveNarrative, AdaptiveNarrative)
import Yaifl.Text.DynamicText
import Yaifl.Text.ListWriter
import Yaifl.Text.Print
import Yaifl.Text.ResponseCollection
import Yaifl.Text.Say
import qualified Data.Map as DM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Effectful.Error.Static
import Effectful.Provider.List
import Yaifl.Std.Properties

type PlainWorldModel = 'WorldModel ObjectSpecifics Direction () () () () ActivityCollection ResponseCollection DynamicText


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


type YaiflEffects (wm :: WorldModel) es =
    (ActionHandler wm :> es

    , State (AdaptiveNarrative wm) :> es
    , State (ResponseCollector wm) :> es
    , State (ActivityCollector wm) :> es
    , Input :> es
    , State (ActionCollection wm) :> es
    , ObjectTraverse wm :> es
    , ObjectUpdate wm :> es
    , ObjectLookup wm :> es
    , State Metadata :> es
    , State (WorldActions wm) :> es
    , Print :> es
    , State (World wm) :> es

    , Breadcrumbs :> es
    , Error MissingObject :> es
    , IOE :> es
    , HasStandardProperties wm
    , WMHasObjSpecifics wm
    )

addStandardActions ::
  Display (WMText wm)
  => Breadcrumbs :> es
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
  addAction gettingOff
  pass

blankActions ::
  HasStandardProperties wm
  => WorldActions wm
blankActions = WorldActions
  { actionsMap = DM.empty
  , whenPlayBegins = whenPlayBeginsRules
  , actionProcessing = actionProcessingRules
  , turnSequence = turnSequenceRules
  , everyTurnRules = everyTurnRulesImpl
  , accessibilityRules = accessibility
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
  , parserMatchThreshold = 0.05
  , bufferedInput = []
  , mentionedThings = S.empty
  , rng = mkStdGen 69
  , usePostPromptPbreak = True
  }

newWorld ::
  Pointed (WMObjSpecifics wm)
  => HasLookingProperties wm
  => HasDirectionalTerms wm
  => WMHasObjSpecifics wm
  => WMStdDirections wm
  => Eff (EffStack wm ++ '[IOE]) ()
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
  => Display (WMText wm)
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
  IOE :> es
  => RuleEffects wm es
  => SayableValue (WMText wm) wm
  => State (WorldActions wm) :> es
  => Eff es ()
runTurnsFromBuffer = do
  b <- use @Metadata #bufferedInput
  unless (null b) $ runTurn >> runTurnsFromBuffer

runTurn ::
  forall wm es.
  IOE :> es
  => State (WorldActions wm) :> es
  => SayableValue (WMText wm) wm
  => RuleEffects wm es
  => Eff es ()
runTurn = do
  let actionOpts = ActionOptions False False
  wa <- get @(WorldActions wm)
  -- runRulebook Nothing False (wa ^. #turnSequence) ()
  i <- waitForInput
  print i
  whenJust i $ \actualInput -> do
    printPrompt actionOpts
    withStyle (Just bold) $ printText actualInput
    void $ parseAction actionOpts [NoParameter] actualInput
    void $ runRulebook Nothing False (wa ^. #turnSequence) ()
  -- TODO: this is where every turn things happen
