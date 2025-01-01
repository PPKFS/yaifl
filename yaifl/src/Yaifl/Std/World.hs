module Yaifl.Std.World
  ( World(..)
  , WorldStores(..)
  , addAction
  , addWhenPlayBegins
  ) where

import Yaifl.Prelude

import Yaifl.Core.Entity ( Entity )
import Yaifl.Core.Store
import Yaifl.Core.Metadata ( Metadata )
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.Print ( Has(..), MessageBuffer )
import Yaifl.Core.WorldModel ( WMValues, WorldModel )
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Core.Rules.Rulebook
import Yaifl.Std.Kinds.Region
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Action
import Yaifl.Std.Rulebooks.ActionProcessing
import Breadcrumbs
import Yaifl.Core.Refreshable
import Yaifl.Core.Actions.GoesWith

data World (wm :: WorldModel) = World
  { metadata :: Metadata
  , stores :: WorldStores wm
  , actions :: WorldActions wm
  , activities :: ActivityCollector wm
  , messageBuffer :: MessageBuffer
  , responses :: ResponseCollector wm
  , adaptiveNarrative :: AdaptiveNarrative wm
  } deriving stock (Generic)

data WorldStores (wm :: WorldModel) = WorldStores
  { entityCounter :: (Entity, Entity)
  , things :: Store (Thing wm)
  , rooms :: Store (Room wm)
  , regions :: Store (Region wm)
  , values :: Map Text (WMValues wm)
  , concepts :: ()-- !(Store (AbstractConcept t r c))
  } deriving stock (Generic)

instance Has (World wm) MessageBuffer where
  buf = #messageBuffer

addWhenPlayBegins ::
  State (WorldActions wm) :> es
  => Rule wm Unconstrained () Bool
  -> Eff es ()
addWhenPlayBegins r = #whenPlayBegins %= addRuleLast r

-- | Add an action to the registry.
addAction ::
  (State (WorldActions wm) :> es, Breadcrumbs :> es)
  => Refreshable wm v
  => GoesWith goesWith
  => Action wm resp goesWith v
  -> Eff es ()
addAction ac = do
  addAnnotation $ "Adding an action with the followingly named rules: " <> getAllRules ac
  #actionsMap % at (ac ^. #name) ?= RegularAction (WrappedAction ac)