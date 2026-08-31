module Yaifl.World
  ( World(..)
  , WorldStores(..)
  , addAction
  , whenPlayBegins
  ) where

import Yaifl.Prelude

import Yaifl.Entity ( Entity )
import Yaifl.Store
import Yaifl.Metadata ( Metadata(..) )
import Yaifl.Effects.RuleEffects
import Yaifl.Effects.Print ( Has(..), MessageBuffer )
import Yaifl.WorldModel ( WMValues, WorldModel )
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Rulebook
import Yaifl.Region.Kind
import Yaifl.Thing.Kind
import Yaifl.Room.Kind
import Yaifl.Action
import Yaifl.Rulebooks.ActionProcessing
import Breadcrumbs
import Yaifl.Refreshable
import Yaifl.Actions.GoesWith
import Yaifl.Actions.Args

data World (wm :: WorldModel) = World
  { metadata :: Metadata wm
  , stores :: WorldStores wm
  , actions :: WorldActions wm
  , activities :: ActivityCollector wm
  , messageBuffer :: MessageBuffer
  , responses :: ResponseCollector wm
  , adaptiveNarrative :: AdaptiveNarrative wm
  , values :: ValueCollector wm
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

whenPlayBegins ::
  State (WorldActions wm) :> es
  => Rule wm Unconstrained () Bool
  -> Eff es ()
whenPlayBegins r = #whenPlayBeginsRulebook %= addRuleLast r

-- | Add an action to the registry.
addAction ::
  (State (WorldActions wm) :> es, Breadcrumbs :> es)
  => Refreshable wm v
  => ArgsMightHaveMainObject v (Thing wm)
  => Display v
  => GoesWith goesWith
  => Action wm resp goesWith v
  -> Eff es ()
addAction ac = do
  addAnnotation $ "Adding an action with the followingly named rules: " <> getAllRules ac
  #actionsMap % at (ac ^. #name) ?= RegularAction (WrappedAction ac)