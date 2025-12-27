{-# LANGUAGE RecordWildCards #-}
module Yaifl.ActionOn
  ( actionOnOneThing
  , actionOnNothing
  , oneTouchableThing
  ) where

import Yaifl.Action
import Yaifl.Actions.Args
import Yaifl.Actions.GoesWith
import Yaifl.Thing.Kind
import Yaifl.Prelude


actionOnOneThing ::
  ParseArguments wm (UnverifiedArgs wm 'TakesThingParameter) (Thing wm)
actionOnOneThing = ParseArguments $ \(UnverifiedArgs Args{..}) ->
    return $ SuccessfulParse $ fst variables

actionOnNothing ::
  ParseArguments wm (UnverifiedArgs wm 'TakesNoParameter) ()
actionOnNothing = ParseArguments $ \_ ->
    return $ SuccessfulParse ()

oneTouchableThing ::
  ArgsHaveMainObject (Args wm v) (Thing wm)
  => Args wm v
  -> [Thing wm]
oneTouchableThing a = one $ view argsMainObject a