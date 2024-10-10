module Yaifl.Game.TurnSequence
  ( turnSequenceRules
  , everyTurnRules
  ) where

import Yaifl.Prelude

import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Rules.Run ( runRulebook )
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Action


-- | The rulebook that runs at the start of the game.
turnSequenceRules ::
  Rulebook wm ((:>) (State (WorldActions wm))) () Bool
turnSequenceRules = Rulebook
    "every turn"
    Nothing
    [ notImplementedRule "parse command"
    , notImplementedRule "scene changing"
    , notImplementedRule "make everything unmentioned"
    , makeRule' "every turn" everyTurnRule
    , notImplementedRule "timed events"
    , notImplementedRule "advance time"
    , notImplementedRule "update chronological records"
    ]

everyTurnRule ::
  forall wm es.
  RuleEffects wm es
  => State (WorldActions wm) :> es
  => Eff es (Maybe Bool)
everyTurnRule = do
  wa <- get @(WorldActions wm)
  runRulebook Nothing False (wa ^. #everyTurn) ()

everyTurnRules ::
  Rulebook wm ((:>) (State (WorldActions wm))) () Bool
everyTurnRules = Rulebook
    "every turn"
    Nothing
    [
    ]