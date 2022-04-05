module Yaifl.Actions where

import Yaifl.Common
import Solitude


-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action s where
  Action ::
    { _actionName :: !Text
    , _actionUnderstandAs :: ![Text]
    , _actionParseArguments :: !(ActionParseArguments s v)
    , _actionBeforeRules :: !(ActionRulebook s v)
    , _actionCheckRules :: !(ActionRulebook s v)
    , _actionCarryOutRules :: !(ActionRulebook s v)
    , _actionReportRules :: !(ActionRulebook s v)
    } -> Action s


{-
addAction
  :: Action s
  -> World s
  -> World s
addAction ac =
  actions % at (_actionName ac) ?~ ac

addBaseActions
  :: HasLookingProperties s
  => World s
  -> World s
addBaseActions = foldr (.) id [
    addAction lookingActionImpl
  ]
-}