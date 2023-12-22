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

-- | The standard actions before they are existentially wrapped. This is so we can modify them during
-- world construction as we lose the type information later and cannot modify a `WrappedAction`.
data ActionCollection wm = ActionCollection
  { going :: Action wm ('Optionally ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter)) (GoingActionVariables wm)
  , looking :: Action wm ('Optionally 'TakesConstantParameter) (LookingActionVariables wm)
  , examining :: ExaminingAction wm
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActionCollection