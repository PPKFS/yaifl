module Yaifl.Zork.Actions where

import Yaifl.Prelude

type FindingAction = ()

data ZorkActions = ZorkActions
  { finding :: FindingAction
  } deriving stock (Generic)