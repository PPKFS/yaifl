module Yaifl.Zork.Actions where

import Yaifl.Prelude
import Yaifl.ActionCollection

type FindingAction = ()
type ReadingAction = ()

data ZorkActions wm = ZorkActions
  { baseActions :: ActionCollection wm
  , finding :: FindingAction
  , reading :: ReadingAction
  } deriving stock (Generic)

