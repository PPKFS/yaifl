{-# LANGUAGE UndecidableInstances #-}


module Yaifl.Game.Actions.Collection
  ( ActionCollection(..)
  ) where

import Yaifl.Model.Action
import Yaifl.Game.Actions.Going
import Yaifl.Game.Actions.Looking.Visibility
import Yaifl.Prelude
import Yaifl.Model.Actions.Args
import Yaifl.Game.Actions.Examining
import Yaifl.Game.Actions.Opening
import Yaifl.Game.Actions.Closing
import Yaifl.Game.Actions.Looking
import Yaifl.Game.Actions.SwitchingOn
import Yaifl.Game.Actions.Taking
import Yaifl.Game.Actions.Entering
import Yaifl.Game.Actions.Waiting
import Yaifl.Game.Actions.Exiting (ExitingAction)

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
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActionCollection