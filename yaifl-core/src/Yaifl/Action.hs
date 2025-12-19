{-# LANGUAGE RecordWildCards #-}

module Yaifl.Action
  ( Action(..)
  , ActionRulebook
  , ActionRule
  , ParseArguments(..)
  , ActionPhrase(..)
  , InterpretAs(..)
  , OutOfWorldAction(..)

  , WrappedAction(..)
  , ParseArgumentEffects
  , ParseArgumentResult(..)
  , ActionInterrupt(..)

  , makeActionRulebook
  , actionName
  , getAllRules
  , makeAction

  , withActionInterrupt'
  ) where

import Yaifl.Prelude hiding (Reader)

import Yaifl.WorldModel ( WorldModel )
import Yaifl.Rulebook
import Yaifl.Actions.Args
import Yaifl.Metadata
import Yaifl.Effects.ObjectQuery
import qualified Data.Text as T
import Effectful.Reader.Static
import Yaifl.Thing.Kind
import Effectful.Error.Static
import Yaifl.Actions.GoesWith
import Yaifl.Effects.RuleEffects
import Yaifl.Text.Responses
import Yaifl.Refreshable
import Yaifl.Effects.Print (printLn)

type ParseArgumentEffects wm es = (WithMetadata es, WithoutMissingObjects wm es, RuleEffects wm es)

data ParseArgumentResult wm v =
  FailedParse Text
  | SuccessfulParse v
  | ConversionTo Text [ActionParameter wm]
  deriving stock (Show, Generic, Functor)

-- | `ParseArguments` is the equivalent of Inform7's `set rulebook variables`.
newtype ParseArguments wm ia v = ParseArguments
  { runParseArguments :: forall es. (ParseArgumentEffects wm es, Refreshable wm v) => ia -> Eff es (ParseArgumentResult wm v)
  }

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) resps (goesWith :: ActionSignature) v where
  Action ::
    { name :: Text
    , understandAs :: [Text]
    , matches :: [(Text, ActionSignature)]
    , touchableNouns :: Args wm v -> [Thing wm]
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
    (Refreshable wm v, GoesWith goesWith, Display v)
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

data ActionInterrupt = ContinueAction | StopAction
  deriving stock (Eq, Ord, Enum, Bounded, Generic, Read, Show)

data ActionPhrase (wm :: WorldModel) =
  Interpret (InterpretAs wm)
  | RegularAction (WrappedAction wm)
  | OtherAction (OutOfWorldAction wm)
  deriving stock ( Generic )

-- | If we should interpret some verb as another action (possibly which then points to another interpret as)
data InterpretAs wm = InterpretAs
  { toParseAs :: Text
  , withArgs :: [ActionParameter wm]
  }

makeFieldLabelsNoPrefix ''Action

withActionInterrupt' ::
  Eff (Error ActionInterrupt : es) (Maybe Bool)
  -> Eff es (Maybe Bool)
withActionInterrupt' f = do
  r <- runErrorNoCallStack f
  case r of
    -- TODO: investigate what the callstack adds
    Left ContinueAction -> rulePass
    Left StopAction -> return $ Just False
    Right x -> return x

-- | Get the name of an action. This is mostly here to avoid overlapping instances with label optics and duplicate fields.
actionName :: Lens' (Action wm resp goesWith v) Text
actionName = #name

makeAction ::
  Text
  -> Action wm resp goesWith v
makeAction n = Action
  { name = n
  , understandAs = [n]
  , matches = []
  , touchableNouns = const []
  , responses = \_ -> notImplementedResponse "no response"
  , parseArguments = ParseArguments $ const $ pure $ FailedParse "not parsed"
  , beforeRules = makeActionRulebook ("before " <> n <> " rulebook") []
  , insteadRules = makeActionRulebook ("instead " <> n <> " rulebook") []
  , carryOutRules = makeActionRulebook ("carry out " <> n <> " rulebook")
      [makeRule' "not implemented action rule" $ printLn "This action has not been implemented." >> rulePass]
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

getAllRules ::
  Action wm resp goesWith v
  -> Text
getAllRules Action{..} = T.intercalate "," . mconcat . map getRuleNames $ [ beforeRules, checkRules, carryOutRules, reportRules ]
