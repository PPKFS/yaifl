{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Game.ActionProcessing
  ( actionProcessingRules
  ) where

import Solitude hiding (runReader, Reader)

import Yaifl.Model.Action
import Yaifl.Model.Query
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Rules.Run
import Effectful.Reader.Static


actionProcessingRules :: forall wm. ActionProcessing wm
actionProcessingRules = ActionProcessing $ \aSpan a@((Action{..}) :: Action wm resp goesWith v) u ->
  runReader a $ failHorriblyIfMissing (runRulebook @wm @_ @_ @_ @((:>) (Reader (Action wm resp goesWith v))) (Just aSpan) False (Rulebook @wm
  "action processing"
  (Just True)
  -- I have no idea how this works
  -- coming back to it: nope, even less idea now
  -- a third go over: nope, still no idea
  -- fourth time: thankfully I can just delete it but leave it here for posterity
  --(ParseArguments (\uv -> (\v -> fmap (const v) (unArgs uv)) <$$> (ignoreSpan >> runParseArguments parseArguments uv)))
  [ Rule "before stage rule"
      []
        ( \v -> do
          r <- runRulebookAndReturnVariables (Just aSpan) False beforeRules v
          return (first Just $ fromMaybe (v, Nothing) r))
  , notImplementedRule "carrying requirements rule"
  , notImplementedRule "basic visibility rule"
  ,  Rule "instead stage rule"
      []
        ( \v -> do
          r <- runRulebookAndReturnVariables (Just aSpan) False insteadRules v
          return (first Just $ fromMaybe (v, Nothing) r))
  , notImplementedRule "requested actions require persuasion rule"
  , notImplementedRule "carry out requested actions rule"
  , notImplementedRule "investigate player awareness rule"
  , notImplementedRule "check stage rule"
  , Rule "carry out stage rule"
      []
        ( \v -> do
          r <- runRulebookAndReturnVariables (Just aSpan) False carryOutRules v
          return (first Just $ fromMaybe (v, Nothing) r))
  , Rule "after stage rule"
      []
        ( \v -> do
          r <- runRulebookAndReturnVariables (Just aSpan) False afterRules v
          return (first Just $ fromMaybe (v, Nothing) r))
  , notImplementedRule "investigate player awareness after rule"
  , Rule "report stage rule"
      []
        ( \v -> do
          r <- runRulebookAndReturnVariables (Just aSpan) False reportRules v
          return (first Just $ fromMaybe (v, Nothing) r))
  , notImplementedRule "clean actions rule"
  ]) u)
