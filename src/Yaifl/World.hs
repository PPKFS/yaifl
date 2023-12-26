module Yaifl.World where

import Solitude

import Effectful.Optics ( (%=) )

import Yaifl.Actions.Action ( WorldActions )
import Yaifl.Model.Objects.Entity ( Entity )
import Yaifl.Model.Objects.Store
import Yaifl.Metadata ( Metadata )
import Yaifl.Rules.Rule
import Yaifl.Rules.Rulebook ( addRuleLast )
import Yaifl.Text.Print ( Has(..), MessageBuffer )
import Yaifl.Model.WorldModel ( WMValues, WorldModel )
import Yaifl.Model.Object(Thing, Room)
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Rules.RuleEffects

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