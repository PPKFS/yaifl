{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Actions.Collection
  ( ActionCollection(..)
  ) where

import Yaifl.Actions.Action
import Yaifl.Actions.Going
import Yaifl.Actions.Looking.Visibility
import Solitude

-- | The standard actions before they are existentially wrapped. This is so we can modify them during
-- world construction as we lose the type information later and cannot modify a `WrappedAction`.
data ActionCollection wm = ActionCollection
  { going :: Action wm (GoingActionVariables wm)
  , looking :: Action wm (LookingActionVariables wm)
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActionCollection