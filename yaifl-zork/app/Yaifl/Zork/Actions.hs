module Yaifl.Zork.Actions where

import Yaifl.Prelude
import Yaifl.Action
import Yaifl.Actions.GoesWith
import Yaifl.Thing.Kind

data ZorkActions wm = ZorkActions
  { finding :: FindingAction wm
  , reading :: ReadingAction wm
  } deriving stock (Generic)

baseZorkActions :: ZorkActions wm
baseZorkActions = ZorkActions findingAction (makeAction "reading")

type FindingAction wm = Action wm () 'TakesThingParameter (Thing wm)
type FindingRule wm = ActionRule wm (FindingAction wm) (Thing wm)
findingAction :: FindingAction wm
findingAction = (makeAction "finding")
  { understandAs = ["find", "where is"]

  }

type ReadingAction wm = Action wm () 'TakesThingParameter (Thing wm)
type ReadingRule wm = ActionRule wm (ReadingAction wm) (Thing wm)