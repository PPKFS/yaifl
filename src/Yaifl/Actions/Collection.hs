{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Actions.Collection
  ( ActionCollection(..)
  ) where

import Yaifl.Actions.Action
import Yaifl.Actions.Going
import Yaifl.Actions.Looking.Visibility
import Solitude


data ActionCollection wm = ActionCollection
  { going :: Action wm (GoingActionVariables wm)
  , looking :: Action wm (LookingActionVariables wm)
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActionCollection