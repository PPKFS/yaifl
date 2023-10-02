{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Actions.ActionProcessing
  ( actionProcessingRules
  ) where

import Solitude

import Breadcrumbs
import Yaifl.Actions.Action
import Yaifl.Model.Objects.Query
import Yaifl.Rules.Rule
import Yaifl.Rules.Rulebook
import Yaifl.Rules.Run


actionProcessingRules :: ActionProcessing wm
actionProcessingRules = ActionProcessing $ \aSpan Action{..} u -> withoutMissingObjects (runRulebook (Just aSpan) (Rulebook
  "action processing"
  (Just True)
  -- I have no idea how this works
  -- coming back to it: nope, even less idea now
  -- a third go over: nope, still no idea
  -- fourth time: thankfully I can just delete it but leave it here for posterity
  --(ParseArguments (\uv -> (\v -> fmap (const v) (unArgs uv)) <$$> (ignoreSpan >> runParseArguments parseArguments uv)))
  [ notImplementedRule "Before stage rule"
  , notImplementedRule "carrying requirements rule"
  , notImplementedRule "basic visibility rule"
  , notImplementedRule "instead stage rule"
  , notImplementedRule "requested actions require persuasion rule"
  , notImplementedRule "carry out requested actions rule"
  , notImplementedRule "investigate player awareness rule"
  , notImplementedRule "check stage rule"
  , Rule "carry out stage rule"
      []
        ( \v -> do
          ignoreSpan
          r <- runRulebookAndReturnVariables (Just aSpan) carryOutRules v
          return (first Just $ fromMaybe (v, Nothing) r))
  , notImplementedRule "after stage rule"
  , notImplementedRule "investigate player awareness after rule"
  , notImplementedRule "report stage rule"
  , notImplementedRule "clean actions rule"
  ]) u) (handleMissingObject "" (Just False))
