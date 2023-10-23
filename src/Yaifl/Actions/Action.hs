{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Yaifl.Actions.Action
  ( Action(..)
  , ActionRulebook
  , ActionProcessing(..)
  , ActionParameterType(..)
  , ParseArguments(..)
  , ActionPhrase(..)
  , InterpretAs(..)
  , OutOfWorldAction(..)
  , WorldActions(..)
  , WrappedAction(..)
  , ParseArgumentEffects
  , addAction
  , makeActionRulebook
  , actionName
  , actionsMapL
  ) where

import Solitude

import Breadcrumbs
import Effectful.Optics ( (?=) )
import Yaifl.Rules.Rule
import Yaifl.Rules.Rulebook
import Yaifl.Model.WorldModel ( WorldModel )
import Yaifl.Rules.RuleEffects
import Yaifl.Rules.Args
import Yaifl.Metadata
import Yaifl.Model.Objects.Effects
import qualified Data.Text as T

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
  | TakesConstantParameter
  deriving stock (Show)

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

data OutOfWorldAction wm = OutOfWorldAction
  { name :: Text
  , runOutOfWorldAction :: forall es. RuleEffects wm es => Eff es ()
  }

-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm v = Rulebook wm (Args wm v) Bool

makeFieldLabelsNoPrefix ''Action

-- | Get the name of an action. This is mostly here to avoid overlapping instances with label optics and duplicate fields.
actionName :: Lens' (Action wm v) Text
actionName = #name

-- | If we should interpret some verb as another action (possibly which then points to another interpret as)
data InterpretAs wm = InterpretAs
  { toParseAs :: Text
  , withArgs :: ActionParameter wm
  }

-- | Helper function to make a rulebook of an action; since there are a lot of these for each action,
-- we ignore the span to avoid clutter and thread the arguments through.
makeActionRulebook ::
  Text -- ^ the name of the rule.
  -> [Rule o (Args o v) Bool] -- ^ the list of rules.
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing

data ActionPhrase wm =
  Interpret (InterpretAs wm)
  | RegularAction (WrappedAction wm)
  | OtherAction (OutOfWorldAction wm)
  deriving stock ( Generic )

data WorldActions (wm :: WorldModel) = WorldActions
  { actionsMap :: Map Text (ActionPhrase wm)
  , whenPlayBegins :: Rulebook wm () Bool
  , actionProcessing :: ActionProcessing wm
  } deriving stock ( Generic )

makeFieldLabelsNoPrefix ''WorldActions

actionsMapL :: Lens' (WorldActions wm) (Map Text (ActionPhrase wm))
actionsMapL = #actionsMap

getAllRules ::
  Action wm v
  -> Text
getAllRules Action{..} = T.intercalate "," . mconcat . map getRuleNames $ [ beforeRules, checkRules, carryOutRules, reportRules ]

-- | Add an action to the registry.
addAction ::
  (State (WorldActions wm) :> es, Breadcrumbs :> es)
  => Action wm v
  -> Eff es ()
addAction ac = do
  addAnnotation $ "Adding an action with the followingly named rules: " <> getAllRules ac
  #actionsMap % at (ac ^. #name) ?= RegularAction (WrappedAction ac)