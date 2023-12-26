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
  ) where

import Solitude hiding ( Reader, runReader )


import Effectful.Optics ( (?=) )

import Yaifl.Actions.Action
import Yaifl.Actions.ActionProcessing
import Yaifl.Actions.Going
import Yaifl.Actions.Looking
import Yaifl.Actions.Looking.Locale
import Yaifl.Actions.Looking.Visibility
import Yaifl.Activities.Activity
import Yaifl.Activities.ChoosingNotableLocaleObjects
import Yaifl.Activities.ListingContents
import Yaifl.Activities.PrintingLocaleParagraphAbout
import Yaifl.Activities.PrintingTheLocaleDescription
import Yaifl.Metadata
import Yaifl.Model.Direction
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Object
import Yaifl.Model.ObjectSpecifics
import Yaifl.Model.Objects.Create
import Yaifl.Model.Properties.Container
import Yaifl.Model.Properties.Door
import Yaifl.Model.Properties.Enclosing
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Openable
import Yaifl.Model.WorldModel
import Yaifl.Rules.RuleEffects
import Yaifl.Rules.WhenPlayBegins
import Yaifl.Text.AdaptiveNarrative (blankAdaptiveNarrative)
import Yaifl.Text.Print
import Yaifl.Text.ResponseCollection
import Yaifl.Text.Say
import Yaifl.World

import qualified Data.Map as DM
import qualified Data.Text as T
import Yaifl.Text.ListWriter
import Yaifl.Actions.OutOfWorld
import Yaifl.Rules.Args
import Yaifl.EffectHandlers
import Yaifl.Text.DynamicText
import Yaifl.Actions.Collection
import Breadcrumbs
import Yaifl.Model.Objects.Query (failHorriblyIfMissing)
import Yaifl.Actions.Examining
import Yaifl.Model.Objects.Store
import Yaifl.Actions.Closing
import Yaifl.Actions.Opening

type PlainWorldModel = 'WorldModel ObjectSpecifics Direction () () ActivityCollection ResponseCollection DynamicText

type HasStandardProperties s = (
  WMWithProperty s Enclosing
  , WMWithProperty s Openability
  , WMWithProperty s Container
  , WMWithProperty s Enterable
  , HasLookingProperties s
  , WMStdDirections s
  , WMWithProperty s DoorSpecifics
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
  , groupingTogether :: Activity wm () (AnyObject wm) ()
  , listingContents :: Activity wm () (ListWritingParameters wm) ()
  , listingNondescriptItems :: Activity wm () (AnyObject wm) ()
  , printingANumberOf :: Activity wm () (Int, AnyObject wm) ()
  , printingDescriptionOfADarkRoom :: Activity wm () () ()
  , printingLocaleParagraphAbout :: Activity wm () (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
  , printingNameOfSomething :: Activity wm () (AnyObject wm) Text
  , printingTheLocaleDescription :: Activity wm YouCanAlsoSeeResponses (LocaleVariables wm) ()
  , printingNameOfADarkRoom :: Activity wm () () ()
  -- TODO https://ganelson.github.io/inform/standard_rules/S-act.html#SP15
{- , printingRoomDescriptionDetails :: Activity wm (Thing wm) ()
  , printingInventoryDetails :: Activity wm (Thing wm) ()
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
  pass

blankActions ::
  HasStandardProperties wm
  => WorldActions wm
blankActions = WorldActions
  { actionsMap = DM.empty
  , whenPlayBegins = whenPlayBeginsRules
  , actionProcessing = actionProcessingRules
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
  , roomDescriptions = NoAbbreviatedRoomDescriptions
  , globalTime = 0
  , darknessWitnessed = False
  , currentPlayer = defaultPlayerID
  , currentStage = Construction
  , previousRoom = voidID
  , firstRoom = voidID
  , errorLog = []
  , typeDAG = makeTypeDAG
  , traceAnalysisLevel = Maximal
  , oxfordCommaEnabled = True
  , parserMatchThreshold = 0.66
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

makeTypeDAG :: Map ObjectType (Set ObjectType)
makeTypeDAG = fromList
  [ ("object", fromList [])
  , ("thing", fromList ["object"])
  , ("room", fromList ["object"])
  , ("container", fromList ["thing"])
  , ("supporter", fromList ["thing"])
  , ("door", fromList ["thing"])
  ]

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
    forM_ allTerms $ \term ->
      actionsMapL % at term ?= Interpret (InterpretAs ("go " <> dirN) [NoParameter])

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