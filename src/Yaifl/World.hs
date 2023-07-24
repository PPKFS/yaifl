module Yaifl.World where

import Solitude

import Effectful.Optics ( (%=) )

import Yaifl.Actions.Action ( WorldActions )
import Yaifl.Model.Entity ( Store, Entity )
import Yaifl.Metadata ( Metadata )
import Yaifl.Model.Objects.RoomData ( RoomData )
import Yaifl.Model.Objects.ThingData ( ThingData )
import Yaifl.Rules.Rule
import Yaifl.Rules.Rulebook ( addRuleLast )
import Yaifl.Text.Print ( Has(..), MessageBuffer )
import Yaifl.Model.WorldModel ( WMValues, WorldModel )
import Yaifl.Model.Object(Object)
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
  , things :: Store (Object wm ThingData)
  , rooms :: Store (Object wm (RoomData wm))
  , values :: Map Text (WMValues wm)
  , concepts :: ()-- !(Store (AbstractConcept t r c))
  } deriving stock (Generic)

instance Has (World wm) MessageBuffer where
  buf = #messageBuffer

addWhenPlayBegins ::
  State (WorldActions wm) :> es
  => Rule wm () Bool
  -> Eff es ()
addWhenPlayBegins r = #whenPlayBegins %= addRuleLast r