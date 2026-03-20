{-|
Module      : Yaifl.Activity
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Activities represent mechanical processes carried out by the game engine itself,
in contrast to actions which are executed by actors (players or NPCs).

Key differences from actions:
- Activities have no actor source or controller
- Activities bypass action processing (visibility checks, etc.)

This module defines the activity system:
- `Activity`: Core activity type with before/carry out/after phases
- `ActivityRulebook`: Specialized rulebooks for activity phases
- Activity lifecycle functions: `beginActivity`, `doActivity`, `endActivity`
- Activity construction and management utilities

The activity lifecycle includes:
- Before phase: Setup and initialization
- Carry out phase: Main execution (can be interrupted)
- After phase: Cleanup and finalization
-}

module Yaifl.Activity
  ( -- * Core Types
    Activity(..)
  , ActivityRule
  , ActivityRule'
  , ActivityLens

    -- * Activity Construction
  , blankActivity
  , makeActivity

    -- * Activity Lifecycle
  , beginActivity
  , doActivity
  , endActivity
  , whenHandling
  , whenHandling'

    -- * Activity Utilities
  , afterActivityRules

    -- * Activity Access
  -- | Type-level lens constructor for accessing activities in the world model
  , WithActivity
  -- | Predefined activity accessors for common game activities
  , WithPrintingDescriptionOfADarkRoom
  , WithPrintingNameOfADarkRoom
  , WithListingNondescriptItems
  ) where

import Yaifl.Prelude hiding ( Reader, runReader )

import Breadcrumbs ( withSpan )
import GHC.TypeLits
import Effectful.Reader.Static
import Yaifl.Refreshable ( Refreshable )
import Yaifl.AnyObject
import Yaifl.WorldModel
import Yaifl.Rulebook
import Yaifl.Rulebooks.Run
import Yaifl.Text.Responses
import Yaifl.Effects.RuleEffects
import Yaifl.Text.SayableValue

-- | A nicer wrapper around label optics for activities.
type WithActivity (name :: Symbol) wm resps v r = LabelOptic' name A_Lens (WMActivities wm) (Activity wm resps v r)

type WithPrintingNameOfADarkRoom wm = (WithActivity "printingNameOfADarkRoom" wm () () ())
type WithPrintingDescriptionOfADarkRoom wm = WithActivity "printingDescriptionOfADarkRoom" wm () () ()
type WithListingNondescriptItems wm = WithActivity "listingNondescriptItems" wm () (AnyObject wm) ()

type ActivityRulebook wm resps v re r = Rulebook wm ((:>) (Reader (Activity wm resps v re))) v r
type ActivityRule wm resps v r = ActivityRule' wm resps v r r
type ActivityRule' wm resps v re r = Rule wm ((:>) (Reader (Activity wm resps v re))) v r
-- | The core activity data type representing mechanical game processes.
--
-- An activity encapsulates a complete mechanical process that the game engine can execute.
-- Unlike actions (which are performed by actors), activities are system-level operations.
-- "look" would be an action the player carries out, whereas "printing the name of a room in the dark"
-- would be an activity.
data Activity wm resps v r = Activity
    { name :: Text
    -- ^ Human-readable name of the activity
    , defaultOutcome :: Maybe r
    -- ^ Default result if no rules produce a result
    , currentVariables :: Maybe v
    -- ^ Current state variables (Nothing before execution)
    , responses :: resps -> Response wm v
    -- ^ Function to generate responses from the response collection
    , beforeRules :: ActivityRulebook wm resps v r ()
    -- ^ Rulebook for pre-execution setup and validation
    , carryOutRules :: ActivityRulebook wm resps v r r
    -- ^ Rulebook for main execution logic
    , afterRules :: ActivityRulebook wm resps v r r
    -- ^ Rulebook for post-execution cleanup
    , combineResults :: Maybe r -> Maybe r -> Maybe r
    -- ^ Function to combine multiple rule results
    } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Activity

-- | Lens for accessing the after-rules of an activity.
-- This avoids ambiguity with the overloaded label between the after rules of an Activity and an Action.
afterActivityRules :: Lens' (Activity wm resps v r) (ActivityRulebook wm resps v r r)
afterActivityRules = #afterRules

type ActivityLens wm resps v r = Lens' (WMActivities wm) (Activity wm resps v r)

-- | Create a blank activity with the given name and no rules.
blankActivity ::
  Text
  -> Activity wm resps v r
blankActivity n = makeActivity n []

-- | Create an activity with the given name and carry out rules.
makeActivity ::
  Text
  -> [Rule wm ((:>) (Reader (Activity wm resps v r))) v r]
  -> Activity wm resps v r
makeActivity n rs = Activity n Nothing Nothing (const $ notImplementedResponse "")
  (blankRulebook ("Before " <> n))
  ((blankRulebook ("Carry Out " <> n)) { rules = rs })
  (blankRulebook ("After " <> n))
  const

-- | Begin an activity by running its before rules and setting up variables.
-- This function initiates an activity execution by:
-- 1. Setting the activity's current variables
-- 2. Running the before rules with the provided variables
-- 3. Updating variables based on rule results
beginActivity ::
  forall wm resps v r es.
  RuleEffects wm es
  => SayableValue (WMText wm) wm
  => Display v
  => Refreshable wm v
  => ActivityLens wm resps v r  -- ^ Lens to access the activity in the activities record
  -> v -- ^ Initial variables for the activity
  -> Eff es v -- ^ Final variables state after running before rules
beginActivity acL c = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "begin activity" (ac ^. #name) $ \aSpan ->
      (do
        modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ c)
        -- run the before rules only.
        r <- runReader ac $ runRulebookAndReturnVariables (Just aSpan) True (beforeRules ac) c
        whenJust r $ \r' -> modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ fst r')
        pure $ maybe c fst r)

-- | Variant of `whenHandling` that doesn't take activity variables.
-- This function starts an activity and executes it. If the activity
-- completes without producing a result (returns Nothing), it then
-- executes the provided action.
whenHandling' ::
  RuleEffects wm es
  => SayableValue (WMText wm) wm
  => Display v
  => Display r
  => Refreshable wm v
  => ActivityLens wm resps v r  -- ^ Lens to access the activity
  -> Eff es a                     -- ^ Fallback action to execute if activity has no result
  -> Eff es (Either a (Maybe r)) -- ^ Either the fallback result or activity result
whenHandling' acF f = whenHandling acF (const f)

-- | Execute an activity's carry-out rules with conditional fallback behavior.
-- If the carry-out rules produce no result, executes the provided function.
-- If the carry-out rules produce a result, returns that result without executing the function.
whenHandling ::
  forall wm resps v r a es.
  RuleEffects wm es
  => SayableValue (WMText wm) wm
  => Display v
  => Display r
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> (v -> Eff es a)
  -> Eff es (Either a (Maybe r))
whenHandling acL f = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "handling activity" (ac ^. #name) $ \aSpan ->
    case currentVariables ac of
      Nothing -> pure (Right Nothing)
      Just c -> do
        r <- runReader ac $ runRulebookAndReturnVariables (Just aSpan) True (carryOutRules ac) c
        modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ maybe c fst r)
        let runBlock = do
              a <- f c
              pure $ Left a
        case r of
        -- no result, so run our block
          Nothing -> runBlock
          -- no result but we did update our variables
          Just (v, Nothing) -> do
            modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ v)
            runBlock
          Just (_, Just x) -> pure (Right (Just x))

-- | End an activity by running its after rules and cleaning up variables.
-- This function finalizes an activity execution by:
-- 1. Running the after rules with the current variables
-- 2. Cleaning up the activity's current variables
-- 3. Returning the final variables state
endActivity ::
  forall wm resps v r es.
  HasCallStack
  => RuleEffects wm es
  => SayableValue (WMText wm) wm
  => Display v
  => Display r
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> Eff es (Maybe v)
endActivity acF = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acF)
  withSpan "end activity" (ac ^. #name) $ \aSpan ->
      (do
        case currentVariables ac of
          Nothing -> pure Nothing
          Just c -> do
            r <- runReader ac $ runRulebookAndReturnVariables (Just aSpan) True (afterRules ac) c
            modify @(ActivityCollector wm) (#activityCollection % acF % #currentVariables .~ Nothing)
            pure $ maybe (Just c) (Just . fst) r)

-- | Execute a complete activity lifecycle: before, carry-out, and after phases.
-- This function runs the full activity execution pipeline:
-- 1. Sets up initial variables
-- 2. Runs before rules (setup/validation)
-- 3. Runs carry out rules (main execution)
-- 4. Runs after rules (cleanup)
-- 5. Cleans up variables and returns the final result
doActivity ::
  forall wm resps r v es.
  (RuleEffects wm es, Display r, Display v)
  => SayableValue (WMText wm) wm
  => Refreshable wm v
  => ActivityLens wm resps v r  -- ^ Lens to access the activity
  -> v -- ^ Initial variables
  -> Eff es (Maybe r) -- ^ Final result (Nothing if no result produced)
doActivity acL c = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "activity" (ac ^. #name) $ \aSpan -> runReader ac (do
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ c)
    x <- runRulebookAndReturnVariables (Just aSpan) True (beforeRules ac) c
    mr <- runRulebookAndReturnVariables (Just aSpan) True (carryOutRules ac) (maybe c fst x)
    er <- runRulebookAndReturnVariables (Just aSpan) True (afterRules ac) (maybe c fst mr)
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables .~ Nothing)
    return $ combineResults ac (snd =<< mr) (snd =<< er))
