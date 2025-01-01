{-# LANGUAGE UndecidableInstances #-}


module Yaifl.Std.Actions.Collection
  ( ActionCollection(..)
  ) where

import Yaifl.Core.Action
import Yaifl.Std.Actions.Going
import Yaifl.Std.Actions.Looking.Visibility
import Yaifl.Prelude
import Yaifl.Std.Actions.Examining
import Yaifl.Std.Actions.Opening
import Yaifl.Std.Actions.Closing
import Yaifl.Std.Actions.Looking
import Yaifl.Std.Actions.SwitchingOn
import Yaifl.Std.Actions.Taking
import Yaifl.Std.Actions.Entering
import Yaifl.Std.Actions.Waiting
import Yaifl.Std.Actions.Exiting (ExitingAction)
import Yaifl.Std.Actions.GettingOff (GettingOffAction)
import Yaifl.Core.Actions.GoesWith

-- | The standard actions before they are existentially wrapped. This is so we can modify them during
-- world construction as we lose the type information later and cannot modify a `WrappedAction`.
data ActionCollection wm = ActionCollection
  { going :: Action wm GoingResponses ('Optionally ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter)) (GoingActionVariables wm)
  , looking :: Action wm (LookingResponses wm) ('Optionally 'TakesConstantParameter) (LookingActionVariables wm)
  , examining :: ExaminingAction wm
  , opening :: OpeningAction wm
  , closing :: ClosingAction wm
  , switchingOn :: SwitchingOnAction wm
  , taking :: TakingAction wm
  , entering :: EnteringAction wm
  , waiting :: WaitingAction wm
  , exiting :: ExitingAction wm
  , gettingOff :: GettingOffAction wm
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActionCollection