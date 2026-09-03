{-# LANGUAGE RecordWildCards #-}
module Yaifl.Zork.Actions where

import Yaifl.Prelude
import Yaifl.Action (makeActionRulebook)
import Yaifl.Actions.GoesWith
import Yaifl.Actions.Args (Args(..))
import Yaifl.Thing.Kind
import Yaifl.ActionOn (actionOnOneThing)
import Yaifl.Rulebook (makeRule, rulePass)
import Yaifl.Text.Say
import Yaifl.Action

data ZorkActions wm = ZorkActions
  { finding :: FindingAction wm
  , reading :: ReadingAction wm
  , raising :: RaisingAction wm
  } deriving stock (Generic)

baseZorkActions :: WithPrintingNameOfSomething wm => ZorkActions wm
baseZorkActions = ZorkActions findingAction readingAction raisingAction

type FindingAction wm = Action wm () 'TakesThingParameter (Thing wm)
type FindingRule wm = ActionRule wm (FindingAction wm) (Thing wm)
findingAction :: FindingAction wm
findingAction = (makeAction "finding")
  { understandAs = ["find", "where is"]
  , parseArguments = actionOnOneThing
  }

type ReadingAction wm = Action wm () 'TakesThingParameter (Thing wm)
type ReadingRule wm = ActionRule wm (ReadingAction wm) (Thing wm)

readingAction :: ReadingAction wm
readingAction = (makeAction "reading")
  { understandAs = ["read", "reading"]
  , parseArguments = actionOnOneThing
  }

type RaisingAction wm = Action wm () 'TakesThingParameter (Thing wm)
type RaisingRule wm = ActionRule wm (RaisingAction wm) (Thing wm)

standardRaising :: WithPrintingNameOfSomething wm => RaisingRule wm
standardRaising = makeRule "standard raising rule" [] $ \Args{..} -> do
  [saying|You can't raise {the variables}.|]
  rulePass

raisingAction :: WithPrintingNameOfSomething wm => RaisingAction wm
raisingAction = (makeAction "raising")
  { understandAs = ["raise"]
  , parseArguments = actionOnOneThing
  , carryOutRules = makeActionRulebook "carry out raising" [standardRaising]
  }