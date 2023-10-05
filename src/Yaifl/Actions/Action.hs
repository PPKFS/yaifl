{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Actions.Action
  ( Action(..)
  , ActionRulebook
  , ActionProcessing(..)
  , ActionParameterType(..)
  , ParseArguments(..)
  , InterpretAs(..)
  , WorldActions(..)
  , WrappedAction(..)
  , ParseArgumentEffects
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
import Yaifl.Rules.Args
import Yaifl.Metadata
import Yaifl.Model.Objects.Effects

newtype ActionProcessing wm = ActionProcessing
  (forall es v.
    RuleEffects wm es
    => Refreshable wm v
    => SpanID
    -> Action wm v
    -> Args wm v
    -> Eff es (Maybe Bool)
  )

type ParseArgumentEffects wm es = (WithMetadata es, NoMissingObjects wm es, RuleEffects wm es)

-- | `ParseArguments` is the equivalent of Inform7's `set rulebook variables`.
newtype ParseArguments wm ia v = ParseArguments
  { runParseArguments :: forall es. (ParseArgumentEffects wm es, Refreshable wm v) => ia -> Eff es (Either Text v)
  }

data ActionParameterType =
  TakesNoParameter
  | Optionally ActionParameterType
  | TakesDirectionParameter
  | TakesObjectParameter
  | TakesOneOf ActionParameterType ActionParameterType

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) v where
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
    } -> Action wm v
  deriving stock (Generic)

data WrappedAction (wm :: WorldModel) where
  WrappedAction ::
    Action wm v
    -> WrappedAction wm

-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm v = Rulebook wm (Args wm v) Bool

makeFieldLabelsNoPrefix ''Action

-- | Get the name of an action. This is mostly here to avoid overlapping instances with label optics and duplicate fields.
actionName :: Lens' (Action wm v) Text
actionName = #name

-- | If we should interpret some verb as another action (possibly which then points to another interpret as)
newtype InterpretAs = InterpretAs Text deriving stock (Eq, Show)

-- | Helper function to make a rulebook of an action; since there are a lot of these for each action,
-- we ignore the span to avoid clutter and thread the arguments through.
makeActionRulebook ::
  Text -- ^ the name of the rule.
  -> [Rule o (Args o v) Bool] -- ^ the list of rules.
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing

data WorldActions (wm :: WorldModel) = WorldActions
  { actions :: Map Text (Either InterpretAs (WrappedAction wm))
  , whenPlayBegins :: Rulebook wm () Bool
  , actionProcessing :: ActionProcessing wm
  } deriving stock ( Generic )

makeFieldLabelsNoPrefix ''WorldActions

-- | Run an action. This assumes that all parsing has been completed.
runAction ::
  forall wm es.
  State (WorldActions wm) :> es
  => RuleEffects wm es
  => UnverifiedArgs wm
  -> WrappedAction wm
  -> Eff es (Maybe Bool)
runAction uArgs (WrappedAction act) = withSpan "run action" (act ^. #name) $ \aSpan -> do
  mbArgs <- (\v -> fmap (const v) (unArgs uArgs)) <$$> (runParseArguments (act ^. #parseArguments) uArgs)
  case mbArgs of
    Left err -> noteError (const $ Just False) err
    Right args -> do
      -- running an action is simply evaluating the action processing rulebook.
      (ActionProcessing ap) <- use @(WorldActions wm) #actionProcessing
      ap aSpan act args

-- | Add an action to the registry.
addAction ::
  State (WorldActions wm) :> es
  => Action wm v
  -> Eff es ()
addAction ac = #actions % at (ac ^. #name) ?= Right (WrappedAction ac)