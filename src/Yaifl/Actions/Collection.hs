module Yaifl.Actions.Collection
  ( ActionCollection(..)
  ) where

import Yaifl.Actions.Action
import Yaifl.Actions.Going
import Yaifl.Actions.Looking.Visibility


data ActionCollection wm = ActionCollection
  { going :: Action wm (GoingActionVariables wm)
  , looking :: Action wm (LookingActionVariables wm)
  }