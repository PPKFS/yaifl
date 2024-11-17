{-# LANGUAGE RecordWildCards #-}

module Yaifl.Model.Action
  ( Action(..)
  , ActionRulebook
  , ActionRule
  , ActionProcessing(..)
  , ParseArguments(..)
  , ActionPhrase(..)
  , InterpretAs(..)
  , OutOfWorldAction(..)
  , WorldActions(..)
  , WrappedAction(..)
  , ParseArgumentEffects
  , ParseArgumentResult(..)
  , addAction
  , makeActionRulebook
  , actionName
  , actionsMapL
  , actionOnOneThing
  , actionOnNothing
  , makeAction
  ) where

import Yaifl.Prelude hiding (Reader)

import Breadcrumbs
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.WorldModel ( WorldModel )
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Actions.Args
import Yaifl.Model.Metadata
import Yaifl.Model.Effects
import qualified Data.Text as T
import Yaifl.Text.Responses
import Effectful.Reader.Static
import Yaifl.Model.Kinds.Thing

newtype ActionProcessing wm = ActionProcessing
  (forall es resp goesWith v.
    RuleEffects wm es
    => Refreshable wm v
    => SpanID
    -> Action wm resp goesWith v
    -> Args wm v
    -> Eff es (Maybe Bool)
  )

newtype InsteadRules wm = InsteadRules
  (forall es resp goesWith v.
    RuleEffects wm es
    => Refreshable wm v
    => SpanID
    -> Action wm resp goesWith v
    -> Args wm v
    -> Eff es Bool
  )

type ParseArgumentEffects wm es = (WithMetadata es, NoMissingObjects wm es, RuleEffects wm es)

data ParseArgumentResult v =
  FailedParse Text
  | SuccessfulParse v
  | ConversionTo Text
  deriving stock (Eq, Ord, Show, Generic, Functor)

-- | `ParseArguments` is the equivalent of Inform7's `set rulebook variables`.
newtype ParseArguments wm ia v = ParseArguments
  { runParseArguments :: forall es. (ParseArgumentEffects wm es, Refreshable wm v) => ia -> Eff es (ParseArgumentResult v)
  }

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) resps (goesWith :: ActionParameterType) v where
  Action ::
    { name :: Text
    , understandAs :: [Text]
    , matches :: [(Text, ActionParameterType)]
    , responses :: resps -> Response wm (Args wm v)
    , parseArguments :: ParseArguments wm (UnverifiedArgs wm goesWith) v
    , beforeRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , insteadRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , checkRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , carryOutRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , reportRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , afterRules :: ActionRulebook wm (Action wm resps goesWith v) v
    } -> Action wm resps goesWith v
  deriving stock (Generic)

data WrappedAction (wm :: WorldModel) where
  WrappedAction ::
    GoesWith goesWith
    => Action wm resp goesWith v
    -> WrappedAction wm

data OutOfWorldAction wm = OutOfWorldAction
  { name :: Text
  , runOutOfWorldAction :: forall es. RuleEffects wm es => Eff es ()
  }

-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm ac v = Rulebook wm ((:>) (Reader ac)) (Args wm v) Bool
type ActionRule wm ac v = Rule wm ((:>) (Reader ac)) (Args wm v) Bool
makeFieldLabelsNoPrefix ''Action

-- | Get the name of an action. This is mostly here to avoid overlapping instances with label optics and duplicate fields.
actionName :: Lens' (Action wm resp goesWith v) Text
actionName = #name

-- | If we should interpret some verb as another action (possibly which then points to another interpret as)
data InterpretAs wm = InterpretAs
  { toParseAs :: Text
  , withArgs :: [NamedActionParameter wm]
  }

makeAction ::
  Text
  -> Action wm resp goesWith v
makeAction n = Action
  { name = n
  , understandAs = [n]
  , matches = []
  , responses = \_ -> notImplementedResponse "no response"
  , parseArguments = ParseArguments $ const $ pure $ FailedParse "not parsed"
  , beforeRules = makeActionRulebook ("before " <> n <> " rulebook") []
  , insteadRules = makeActionRulebook ("instead " <> n <> " rulebook") []
  , carryOutRules = makeActionRulebook ("carry out " <> n <> " rulebook") []
  , afterRules = makeActionRulebook ("after " <> n <> " rulebook") []
  , checkRules = makeActionRulebook ("check " <> n <> " rulebook") []
  , reportRules = makeActionRulebook ("report " <> n <> " rulebook") []
  }
-- | Helper function to make a rulebook of an action; since there are a lot of these for each action,
-- we ignore the span to avoid clutter and thread the arguments through.
makeActionRulebook ::
  Text -- ^ the name of the rule.
  -> [Rule wm ((:>) (Reader (Action wm resps goesWith v))) (Args wm v) Bool] -- ^ the list of rules.
  -> ActionRulebook wm (Action wm resps goesWith v) v
makeActionRulebook n = Rulebook n Nothing

data ActionPhrase (wm :: WorldModel) =
  Interpret (InterpretAs wm)
  | RegularAction (WrappedAction wm)
  | OtherAction (OutOfWorldAction wm)
  deriving stock ( Generic )

data WorldActions (wm :: WorldModel) = WorldActions
  { actionsMap :: Map Text (ActionPhrase wm)
  , whenPlayBegins :: Rulebook wm Unconstrained () Bool
  , turnSequence :: Rulebook wm ((:>) (State (WorldActions wm))) () Bool
  , everyTurn :: Rulebook wm ((:>) (State (WorldActions wm))) () Bool
  , actionProcessing :: ActionProcessing wm
  } deriving stock ( Generic )

makeFieldLabelsNoPrefix ''WorldActions

actionsMapL :: Lens' (WorldActions wm) (Map Text (ActionPhrase wm))
actionsMapL = #actionsMap

getAllRules ::
  Action wm resp goesWith v
  -> Text
getAllRules Action{..} = T.intercalate "," . mconcat . map getRuleNames $ [ beforeRules, checkRules, carryOutRules, reportRules ]

-- | Add an action to the registry.
addAction ::
  (State (WorldActions wm) :> es, Breadcrumbs :> es)
  => GoesWith goesWith
  => Action wm resp goesWith v
  -> Eff es ()
addAction ac = do
  addAnnotation $ "Adding an action with the followingly named rules: " <> getAllRules ac
  #actionsMap % at (ac ^. #name) ?= RegularAction (WrappedAction ac)

actionOnOneThing ::
  ParseArguments wm (UnverifiedArgs wm 'TakesThingParameter) (Thing wm)
actionOnOneThing = ParseArguments $ \(UnverifiedArgs Args{..}) ->
    return $ SuccessfulParse $ fst variables

actionOnNothing ::
  ParseArguments wm (UnverifiedArgs wm 'TakesNoParameter) ()
actionOnNothing = ParseArguments $ \_ ->
    return $ SuccessfulParse ()