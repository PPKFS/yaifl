module Yaifl.Actions
  ( addBaseActions
  )
where

import Yaifl.Common
import Yaifl.Prelude
import Yaifl.Actions.Looking

addAction
  :: Action s
  -> World s
  -> World s
addAction ac =
  actions % at (_actionName ac) ?~ ac

addBaseActions
  :: ()--HasLookingProperties s
  => World s
  -> World s
addBaseActions = foldr (.) id [
    --addAction lookingActionImpl
  ]