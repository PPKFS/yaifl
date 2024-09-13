module Yaifl.Game.World where

import Yaifl.Prelude

import Yaifl.Model.Action ( WorldActions )
import Yaifl.Model.Entity ( Entity )
import Yaifl.Model.Store
import Yaifl.Model.Metadata ( Metadata )
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Print ( Has(..), MessageBuffer )
import Yaifl.Model.WorldModel ( WMValues, WorldModel )
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Kinds.Region
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Room

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