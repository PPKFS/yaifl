# Actions

```haskell file=src/Yaifl/Core/Actions/Action.hs
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Actions.Action 
  ( Action(..)
  --, parseAction
  , ActionParseArguments
  , ActionRulebook
  , ActionProcessing(..)

  , WorldActions(..)

  , whenPlayBegins
  --, getAction
  --, tryAction
  --, addAction
  , makeActionRulebook
  ) where

import Solitude
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Common
import Yaifl.Core.Rulebooks.Rule
import Cleff.State

-- | The type of argument parsing for actions. The important part here is that we
-- parse to `v` rather than to `Args s v` to better move between rulebooks.
type ActionParseArguments wm v = ParseArguments wm (UnverifiedArgs wm) v

newtype ActionProcessing wm = ActionProcessing (forall es. RuleEffects wm es => Action wm -> UnverifiedArgs wm -> Eff es (Maybe Bool))

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) where
  Action ::
    { _actionName :: !Text
    , _actionUnderstandAs :: ![Text]
    , _actionMatching :: ![Text]
    , _actionParseArguments :: !(ActionParseArguments wm v)
    , _actionBeforeRules :: !(ActionRulebook wm v)
    , _actionCheckRules :: !(ActionRulebook wm v)
    , _actionCarryOutRules :: !(ActionRulebook wm v)
    , _actionReportRules :: !(ActionRulebook wm v)
    } -> Action wm


-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm v = Rulebook wm (Args wm v) (Args wm v) Bool

-- | Helper function to make a rulebook of an action.
makeActionRulebook :: 
  Text
  -> [Rule o (Args o v) Bool]
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing (ParseArguments $ \x -> return $ Just x)


data WorldActions (wm :: WorldModel) = WorldActions
  { _actions :: !(Map Text (Action wm))
  , _activities :: () -- !(ActivityCollection wm)
  , _whenPlayBegins :: !(Rulebook wm () () Bool)
  , _actionProcessing :: ActionProcessing wm
  }

makeLenses ''WorldActions

-- | Run an action. This assumes that all parsing has been completed.
runAction :: 
  State (WorldActions wm) :> es
  => RuleEffects wm es
  => UnverifiedArgs wm
  -> Action wm 
  -> Eff es (Maybe Bool)
runAction args act = do
  w <- use actionProcessing
  let (ActionProcessing ap) = w
  ap act args

-- | Lookup an action from the world. TODO: handle "did you mean", synonyms, etc.
getAction :: 
  State (WorldActions wm) :> es
  => Text
  -> UnverifiedArgs wm
  -> Eff es (Maybe (Action wm))
getAction n _ = use $ actions % at n

-- | Add an action to the registry.
addAction :: 
  State (WorldActions wm) :> es
  => Action wm 
  -> Eff es ()
addAction ac =
  actions % at (_actionName ac) ?= ac

```
