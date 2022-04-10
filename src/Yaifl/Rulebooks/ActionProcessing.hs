{-|
Module      : Yaifl.Rulebooks.ActionProcessing
Description : The default action processing rulebook.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Rulebooks.ActionProcessing 
  ( defaultActionProcessingRules
  ) where

import Solitude
import Yaifl.Actions.Action
import Yaifl.Rulebooks.Rulebook
import Yaifl.Objects.Missing


defaultActionProcessingRules :: ActionProcessing wm
defaultActionProcessingRules = ActionProcessing $ \Action{..} u -> withoutMissingObjects (runRulebook (Rulebook
  "action processing"
  (Just True)
  -- I have no idea how this works
  -- coming back to it: nope, even less idea now
  (ParseArguments (\uv -> (\v -> fmap (const v) (unArgs uv)) <$$> runParseArguments _actionParseArguments uv))
  [ notImplementedRule "Before stage rule"
  , notImplementedRule "carrying requirements rule"
  , notImplementedRule "basic visibility rule"
  , notImplementedRule "instead stage rule"
  , notImplementedRule "requested actions require persuasion rule"
  , notImplementedRule "carry out requested actions rule"
  , notImplementedRule "investigate player awareness rule"
  , notImplementedRule "check stage rule"
  , Rule "carry out stage rule"
        ( \v -> do
          r <- runRulebookAndReturnVariables _actionCarryOutRules v
          return (fromMaybe (v, Nothing) r))
  , notImplementedRule "after stage rule"
  , notImplementedRule "investigate player awareness after rule"
  , notImplementedRule "report stage rule"
  , notImplementedRule "clean actions rule"
  ]) u) (handleMissingObject "" (Just False))