{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Yaifl.Model.Entity
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

type PlainWorldModel = 'WorldModel ObjectSpecifics Direction () () ActivityCollection ResponseCollection DynamicText

--instance HasDirectionalTerms PlainWorldModel where
 -- toTextDir = toTextDir

type HasStandardProperties s = (
  WMHasProperty s Enclosing
  , WMHasProperty s Container
  , WMHasProperty s Enterable
  , WMHasProperty s Openable
  , HasLookingProperties s
  , WMStdDirections s
  , WMHasProperty s DoorSpecifics
  , HasDirectionalTerms s
  , Pointed (WMObjSpecifics s)
  )

data ActivityCollection wm = ActivityCollection
  { printingNameOfADarkRoom :: !(Activity wm () ())
  , printingNameOfSomething :: !(Activity wm (AnyObject wm) Text)
  , printingDescriptionOfADarkRoom :: !(Activity wm () ())
  , choosingNotableLocaleObjects :: !(Activity wm (AnyObject wm) (LocalePriorities wm))
  , printingLocaleParagraphAbout :: !(Activity wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm))
  , printingTheLocaleDescription :: !(Activity wm (LocaleVariables wm) ())
  , listingNondescriptItems :: !(Activity wm (AnyObject wm) ())
  , listingContents :: !(Activity wm (ListWritingParameters wm) ())
  , groupingTogether :: Activity wm (AnyObject wm) ()
  , printingANumberOf :: Activity wm (Int, AnyObject wm) ()
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
      actionsMapL % at term ?= Interpret (InterpretAs ("go " <> dirN) NoParameter)

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