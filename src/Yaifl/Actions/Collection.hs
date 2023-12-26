{-# LANGUAGE UndecidableInstances #-}


module Yaifl.Actions.Collection
  ( ActionCollection(..)
  ) where

import Yaifl.Actions.Action
import Yaifl.Actions.Going
import Yaifl.Actions.Looking.Visibility
import Solitude
import Yaifl.Rules.Args
import Yaifl.Actions.Examining
import Yaifl.Actions.Opening
import Yaifl.Actions.Closing
import Yaifl.Actions.Looking

-- | The standard actions before they are existentially wrapped. This is so we can modify them during
-- world construction as we lose the type information later and cannot modify a `WrappedAction`.
data ActionCollection wm = ActionCollection
  { going :: Action wm GoingResponses ('Optionally ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter)) (GoingActionVariables wm)
  , looking :: Action wm (LookingResponses wm) ('Optionally 'TakesConstantParameter) (LookingActionVariables wm)
  , examining :: ExaminingAction wm
  , opening :: OpeningAction wm
  , closing :: ClosingAction wm
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActionCollection