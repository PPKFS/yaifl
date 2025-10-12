{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Std.Rulebooks.ActionProcessing
  ( ActionProcessing(..)
  , actionProcessingRules
  , WorldActions(..)
  , actionsMapL
  , runAction
  ) where

import Yaifl.Prelude hiding (runReader, Reader)

import Yaifl.Core.Action
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.SayableValue
import Yaifl.Core.Rules.Run
import Effectful.Reader.Static
import Breadcrumbs
import Yaifl.Core.Actions.Args
import Yaifl.Core.Effects
import Yaifl.Core.WorldModel
import Yaifl.Core.Rules.Rulebook
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Refreshable


data WorldActions (wm :: WorldModel) = WorldActions
  { actionsMap :: Map Text (ActionPhrase wm)
  , whenPlayBegins :: Rulebook wm Unconstrained () Bool
  , turnSequence :: Rulebook wm ((:>) (State (WorldActions wm))) () Bool
  , everyTurnRules :: Rulebook wm ((:>) (State (WorldActions wm))) () Bool
  , actionProcessing :: ActionProcessing wm
  , accessibilityRules :: Rulebook wm Unconstrained (Args wm (Thing wm)) Bool
  } deriving stock ( Generic )


actionsMapL :: Lens' (WorldActions wm) (Map Text (ActionPhrase wm))
actionsMapL = #actionsMap

newtype ActionProcessing wm = ActionProcessing
  (forall es resp goesWith v.
    RuleEffects wm es
    => State (WorldActions wm) :> es
    => Refreshable wm v
    => Display v
    => Maybe SpanID
    -> Action wm resp goesWith v
    -> Args wm v
    -> Eff es (Maybe Bool)
  )


actionProcessingRules :: forall wm. SayableValue (WMText wm) wm => ActionProcessing wm
actionProcessingRules = ActionProcessing $ \aSpan a@((Action{..}) :: Action wm resp goesWith v) u -> do
  wa <- get @(WorldActions wm)
  runReader a $ failHorriblyIfMissing (runRulebook @wm @_ @_ @_ @((:>) (Reader (Action wm resp goesWith v))) aSpan False (Rulebook @wm
    "action processing"
    (Just True)
    -- I have no idea how this works
    -- coming back to it: nope, even less idea now
    -- a third go over: nope, still no idea
    -- fourth time: thankfully I can just delete it but leave it here for posterity
    --(ParseArguments (\uv -> (\v -> fmap (const v) (unArgs uv)) <$$> (ignoreSpan >> runParseArguments parseArguments uv)))
    [ Rule "before stage rule" []
          ( \v -> do
            ignoreSpanIfEmptyRulebook beforeRules
            r <- runRulebookAndReturnVariables (aSpan) False beforeRules v
            return (first Just $ fromMaybe (v, Nothing) r))
    , notImplementedRule "carrying requirements rule"
    , notImplementedRule "basic visibility rule"
    , Rule "basic accessibility rule" []
        ( \v -> do
          let accessibility = wa ^. #accessibilityRules
          ignoreSpanIfEmptyRulebook accessibility
          -- if any of the objects are not touchable then we give up
          r <- and . catMaybes <$> mapM
            (\n -> runRulebook (aSpan) False accessibility (v {variables = n})) (touchableNouns v)
          if r then return (Just v, Nothing) else return (Just v, Just False)
        )
    , Rule "instead stage rule" []
          ( \v -> do
            ignoreSpanIfEmptyRulebook insteadRules
            r <- runRulebookAndReturnVariables (aSpan) False insteadRules v
            return (first Just $ fromMaybe (v, Nothing) r))
    , notImplementedRule "requested actions require persuasion rule"
    , notImplementedRule "carry out requested actions rule"
    , notImplementedRule "investigate player awareness rule"
    , Rule "check stage rule"
        []
          ( \v -> do
            ignoreSpanIfEmptyRulebook checkRules
            r <- runRulebookAndReturnVariables (aSpan) False checkRules v
            return (first Just $ fromMaybe (v, Nothing) r))
    , Rule "carry out stage rule"
        []
          ( \v -> do
            ignoreSpanIfEmptyRulebook carryOutRules
            r <- runRulebookAndReturnVariables (aSpan) False carryOutRules v
            return (first Just $ fromMaybe (v, Nothing) r))
    , Rule "after stage rule"
        []
          ( \v -> do
            ignoreSpanIfEmptyRulebook afterRules
            r <- runRulebookAndReturnVariables (aSpan) False afterRules v
            return (first Just $ fromMaybe (v, Nothing) r))
    , notImplementedRule "investigate player awareness after rule"
    , Rule "report stage rule"
        []
          ( \v -> do
            addAnnotation $ show (silently (actionOptions v))
            if silently (actionOptions v)
            then return (Just v, Nothing)
            else do
              ignoreSpanIfEmptyRulebook reportRules
              r <- runRulebookAndReturnVariables (aSpan) False reportRules v
              return (first Just $ fromMaybe (v, Nothing) r))
    , notImplementedRule "clean actions rule"
    ]) u)
    where
      ignoreSpanIfEmptyRulebook r = if null (rules r) then ignoreSpan else pass

makeFieldLabelsNoPrefix ''WorldActions

-- | Run an action. This assumes that all parsing has been completed.
runAction ::
  forall wm es goesWith resps v.
  Refreshable wm v
  => Display v
  => State (WorldActions wm) :> es
  => RuleEffects wm es
  => ActionOptions wm
  -> Action wm resps goesWith v
  -> UnverifiedArgs wm goesWith
  -> Eff es Bool
runAction opts act uArgs = withSpan "run action" (act ^. #name) $ \aSpan -> do
  mbArgs <- (\v -> fmap (const v) (unArgs uArgs)) <$$> runParseArguments (act ^. #parseArguments) uArgs
  case mbArgs of
    FailedParse err -> do
      addAnnotation $ "Failed to parse the arguments for the action " <> (act ^. #name) <> " because " <> err
      pure False
    ConversionTo newCommand args -> fromMaybe False . rightToMaybe <$> parseAction opts args newCommand
    SuccessfulParse args -> do
      -- running an action is simply evaluating the action processing rulebook.
      (ActionProcessing ap) <- use @(WorldActions wm) #actionProcessing
      fromMaybe False <$> ap (Just aSpan) act args
