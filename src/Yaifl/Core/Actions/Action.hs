{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Core.Actions.Action
  ( Action(..)
  , ActionParseArguments
  , ActionRulebook
  , ActionProcessing(..)
  , InterpretAs(..)
  , WorldActions(..)
  , addAction
  , runAction
  , makeActionRulebook
  , actionName
  ) where

import Solitude

import Breadcrumbs
import Effectful.Optics ( (?=), use )
import Yaifl.Core.Rulebooks.Rule ( RuleEffects, Rule )
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.WorldModel ( WorldModel )

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
    { name :: Text
    , understandAs :: [Text]
    , matching :: [Text]
    , parseArguments :: ActionParseArguments wm v
    , beforeRules :: ActionRulebook wm v
    , checkRules :: ActionRulebook wm v
    , carryOutRules :: ActionRulebook wm v
    , reportRules :: ActionRulebook wm v
    } -> Action wm
-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm v = Rulebook wm (Args wm v) (Args wm v) Bool

makeFieldLabelsNoPrefix ''Action

actionName ::
  Action wm
  -> Text
actionName = view #name

newtype InterpretAs = InterpretAs Text deriving stock (Eq, Show)

-- | Helper function to make a rulebook of an action.
makeActionRulebook ::
  Text
  -> [Rule o (Args o v) Bool]
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing (ParseArguments $ \x -> ignoreSpan >> pure (Right x))

data WorldActions (wm :: WorldModel) = WorldActions
  { actions :: !(Map Text (Either InterpretAs (Action wm)))
  , whenPlayBegins :: Rulebook wm () () Bool
  , actionProcessing :: ActionProcessing wm
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''WorldActions

-- | Run an action. This assumes that all parsing has been completed.
runAction ::
  forall wm es.
  State (WorldActions wm) :> es
  => RuleEffects wm es
  => UnverifiedArgs wm
  -> Action wm
  -> Eff es (Maybe Bool)
runAction args act = withSpan "run action" (act ^. #name) $ \aSpan -> do
  -- running an action is simply evaluating the action processing rulebook.
  (ActionProcessing ap) <- use @(WorldActions wm) #actionProcessing
  ap aSpan act args

-- | Add an action to the registry.
addAction ::
  State (WorldActions wm) :> es
  => Action wm
  -> Eff es ()
addAction ac = #actions % at (ac ^. #name) ?= Right ac