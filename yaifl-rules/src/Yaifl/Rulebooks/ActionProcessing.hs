{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Yaifl.Rulebooks.ActionProcessing
Copyright   : (c) 2024 Yaifl Project
License     : MIT
Maintainer  : maintainer@yaifl.org
Stability   : stable
Portability : portable

Action processing infrastructure for the Yaifl interactive fiction system.

This module provides the core functionality for processing player actions
through a rule-based system. It coordinates multiple rulebooks, manages
action processing state, and handles the complex interactions between
actions, rules, and world state.

The module is designed around three key concepts:

1. @WorldActions@: Central data structure containing all action processing
   state and rulebook references

2. @ActionProcessing@: Core action processing function with constrained
effects for type-safe operation

3. @Rulebook Integration@: Coordination system for multiple rulebooks
   working together to process actions

See also:
- "Yaifl.Rulebook" for rulebook infrastructure
- "Yaifl.Action" for action definitions
- "Yaifl.Effects.RuleEffects" for rule effect capabilities
- "Yaifl.WorldModel" for world state management
-}

module Yaifl.Rulebooks.ActionProcessing
  ( -- * Core Types
    WorldActions(..)
  , ActionProcessing

  -- * Core Functions
  , actionProcessingRules
  , runAction

  -- * State Management
  , actionsMapL
  ) where

import Yaifl.Prelude hiding (runReader, Reader)

import Yaifl.Action
import Yaifl.Effects.RuleEffects
import Yaifl.Text.SayableValue
import Effectful.Reader.Static
import Breadcrumbs
import Yaifl.Actions.Args
import Yaifl.WorldModel
import Yaifl.Rulebook
import Yaifl.Thing.Kind
import Yaifl.Refreshable
import Yaifl.Text.Say
import Yaifl.Rulebooks.Run

-- | Central data structure containing all action processing state and rulebooks.
--
-- The 'WorldActions' type serves as the central hub for action processing, containing:
--
-- * @actionsMap@: Mapping from action names to action phrases
-- * @whenPlayBegins@: Rulebook for initial setup when play begins
-- * @turnSequence@: Rulebook for turn sequence processing
-- * @everyTurnRules@: Rulebook for rules that run every turn
-- * @actionProcessing@: Core action processing function
-- * @accessibilityRules@: Rulebook for accessibility checks
--
-- This structure enables centralized management of action processing state while
-- maintaining loose coupling between individual rulebooks.
--
-- @
-- Example usage:
--
-- worldActions = WorldActions
--   { actionsMap = initialActions
--   , whenPlayBegins = setupRulebook
--   , turnSequence = turnRulebook
--   , everyTurnRules = perTurnRulebook
--   , actionProcessing = defaultActionProcessing
--   , accessibilityRules = accessibilityRulebook
--   }
-- @

data WorldActions (wm :: WorldModel) = WorldActions
  { actionsMap :: Map Text (ActionPhrase wm)
  , whenPlayBegins :: Rulebook wm Unconstrained () Bool
  , turnSequence :: Rulebook wm ((:>) (State (WorldActions wm))) () Bool
  , everyTurnRules :: Rulebook wm ((:>) (State (WorldActions wm))) () Bool
  , actionProcessing :: ActionProcessing wm
  , accessibilityRules :: Rulebook wm Unconstrained (Args wm (Thing wm)) Bool
  } deriving stock ( Generic )


-- | Lens for accessing the actions map in WorldActions.
--
-- This lens provides convenient access to the 'actionsMap' field of 'WorldActions',
-- allowing for easy updates and queries of the action phrase mappings.
--
-- @
-- Example usage:
--
-- -- Get the current actions map
-- currentMap = worldActions ^. actionsMapL
--
-- -- Update the actions map
-- updatedActions = worldActions & actionsMapL .~ newMap
-- @

actionsMapL :: Lens' (WorldActions wm) (Map Text (ActionPhrase wm))
actionsMapL = #actionsMap

-- | Core action processing function with constrained effects.
--
-- The 'ActionProcessing' newtype wraps a function that processes actions
-- through the Yaifl rule system. It uses a constraint-based approach to
-- ensure type-safe effect handling while providing flexibility for
-- different action requirements.
--
-- The function takes:
-- * An optional 'SpanID' for error reporting
-- * An 'Action' to process
-- * 'Args' containing action arguments
--
-- It returns a 'Maybe Bool' indicating success/failure of the action
-- processing, wrapped in the effect monad 'es'.
--
-- @
-- Example usage:
--
-- processAction :: ActionProcessing wm
-- processAction = ActionProcessing $ \spanId action args -> do
--   -- Process action through rulebooks
--   result <- evaluateActionRules action args
--   -- Handle effects
--   handleRuleEffects result
--   -- Return success
--   return (Just True)
-- @

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
  runReader a (runRulebook @wm @_ @_ @_ @((:>) (Reader (Action wm resp goesWith v))) aSpan False (Rulebook @wm
    "action processing"
    (Just True)
    -- I have no idea how this works
    -- coming back to it: nope, even less idea now
    -- a third go over: nope, still no idea
    -- fourth time: thankfully I can just delete it but leave it here for posterity
    --(ParseArguments (\uv -> (\v -> fmap (const v) (unArgs uv)) <<$>> (ignoreSpan >> runParseArguments parseArguments uv)))
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
  mbArgs <- (\v -> fmap (const v) (unArgs uArgs)) <<$>> runParseArguments (act ^. #parseArguments) uArgs
  case mbArgs of
    FailedParse err -> do
      addAnnotation $ "Failed to parse the arguments for the action " <> (act ^. #name) <> " because " <> err
      when (err == "not parsed") $ let aName = act ^. #name in [saying|The parse arguments for {aName} weren't implemented.|]
      pure False
    ConversionTo newCommand args -> fromMaybe False . rightToMaybe <$> parseAction opts args newCommand
    SuccessfulParse args -> do
      -- running an action is simply evaluating the action processing rulebook.
      (ActionProcessing ap) <- use @(WorldActions wm) #actionProcessing
      fromMaybe False <$> ap (Just aSpan) act args
