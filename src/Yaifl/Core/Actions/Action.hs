{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Actions.Action
  ( Action(..)
  , ActionParseArguments
  , ActionRulebook
  , ActionProcessing(..)
  , InterpretAs(..)
  , WorldActions(..)
  , actionUnderstandAs
  , addAction
  , runAction
  , makeActionRulebook
  ) where

import Solitude

import Effectful.Optics ( (?=), use )

import Yaifl.Core.Rulebooks.Rule ( RuleEffects, Rule )
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.WorldModel ( WorldModel )
import Breadcrumbs

-- | The type of argument parsing for actions. The important part here is that we
-- parse to `v` rather than to `Args s v` to better move between rulebooks.
type ActionParseArguments wm v = ParseArguments wm (UnverifiedArgs wm) v

newtype ActionProcessing wm = ActionProcessing
  (forall es. RuleEffects wm es => SpanID -> Action wm -> UnverifiedArgs wm -> Eff es (Maybe Bool))

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
makeLenses ''Action

newtype InterpretAs = InterpretAs Text deriving stock (Eq, Show)

-- | Helper function to make a rulebook of an action.
makeActionRulebook ::
  Text
  -> [Rule o (Args o v) Bool]
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing (ParseArguments $ \x -> ignoreSpan >> pure (Right x))

data WorldActions (wm :: WorldModel) = WorldActions
  { actions :: !(Map Text (Either InterpretAs (Action wm)))
  , whenPlayBegins :: !(Rulebook wm () () Bool)
  , actionProcessing :: ActionProcessing wm
  } deriving stock (Generic)

-- | Run an action. This assumes that all parsing has been completed.
runAction ::
  forall wm es.
  State (WorldActions wm) :> es
  => RuleEffects wm es
  => UnverifiedArgs wm
  -> Action wm
  -> Eff es (Maybe Bool)
runAction args act = withSpan "run action" (_actionName act) $ \aSpan -> do
  (ActionProcessing ap) <- use @(WorldActions wm) #actionProcessing
  ap aSpan act args

-- | Add an action to the registry.
addAction ::
  State (WorldActions wm) :> es
  => Action wm
  -> Eff es ()
addAction ac = #actions % at (_actionName ac) ?= Right ac