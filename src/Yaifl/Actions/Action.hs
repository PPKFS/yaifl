{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Actions.Action
  ( Action(..)
  , ActionRulebook
  , ActionProcessing(..)
  , ActionParameterType(..)
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
import Yaifl.Rules.Rule
import Yaifl.Rules.Rulebook
import Yaifl.Model.WorldModel ( WorldModel )
import Yaifl.Rules.RuleEffects

newtype ActionProcessing wm = ActionProcessing
  (forall es.
    RuleEffects wm es
    => SpanID
    -> Action wm
    -> UnverifiedArgs wm
    -> Eff es (Maybe Bool)
  )

data ActionParameterType =
  TakesNoParameter
  | Optionally ActionParameterType
  | TakesDirectionParameter
  | TakesObjectParameter
  | TakesOneOf (Either ActionParameterType ActionParameterType)

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) where
  Action ::
    { name :: Text
    , understandAs :: [Text]
    , goesWith :: ActionParameterType
    , matches :: [(Text, ActionParameterType)]
    , parseArguments :: ParseArguments wm (UnverifiedArgs wm) v
    , beforeRules :: ActionRulebook wm v
    , checkRules :: ActionRulebook wm v
    , carryOutRules :: ActionRulebook wm v
    , reportRules :: ActionRulebook wm v
    } -> Action wm

-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm v = Rulebook wm (Args wm v) (Args wm v) Bool

makeFieldLabelsNoPrefix ''Action

-- | Get the name of an action. This is mostly here to avoid overlapping instances with label optics and duplicate fields.
actionName :: Lens' (Action wm) Text
actionName = #name

-- | If we should interpret some verb as another action (possibly which then points to another interpret as)
newtype InterpretAs = InterpretAs Text deriving stock (Eq, Show)

-- | Helper function to make a rulebook of an action; since there are a lot of these for each action,
-- we ignore the span to avoid clutter and thread the arguments through.
makeActionRulebook ::
  Text -- ^ the name of the rule.
  -> [Rule o (Args o v) Bool] -- ^ the list of rules.
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing (ParseArguments $ (ignoreSpan >>) . pure . Right)

data WorldActions (wm :: WorldModel) = WorldActions
  { actions :: Map Text (Either InterpretAs (Action wm))
  , whenPlayBegins :: Rulebook wm () () Bool
  , actionProcessing :: ActionProcessing wm
  } deriving stock ( Generic )

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