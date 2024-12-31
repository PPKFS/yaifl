module Yaifl.Game.TurnSequence
  ( turnSequenceRules
  , everyTurnRulesImpl
  , everyTurn
  ) where

import Yaifl.Prelude

import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Rules.Run ( runRulebook )
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Core.WorldModel
import Yaifl.Game.ActionProcessing


-- | The rulebook that runs at the start of the game.
turnSequenceRules :: SayableValue (WMText wm) wm => Rulebook wm ((:>) (State (WorldActions wm))) () Bool
turnSequenceRules = Rulebook
    "every turn"
    Nothing
    [ notImplementedRule "parse command"
    , notImplementedRule "scene changing"
    , notImplementedRule "make everything unmentioned"
    , makeRule' "every turn" everyTurn
    , notImplementedRule "timed events"
    , notImplementedRule "advance time"
    , notImplementedRule "update chronological records"
    ]

everyTurn ::
  forall wm es.
  RuleEffects wm es
  => SayableValue (WMText wm) wm
  => State (WorldActions wm) :> es
  => Eff es (Maybe Bool)
everyTurn = do
  wa <- get @(WorldActions wm)
  runRulebook Nothing False (wa ^. #everyTurnRules) ()

everyTurnRulesImpl ::
  Rulebook wm ((:>) (State (WorldActions wm))) () Bool
everyTurnRulesImpl = Rulebook
    "every turn"
    Nothing
    [
    ]