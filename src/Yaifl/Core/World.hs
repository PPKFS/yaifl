module Yaifl.Core.World where

import Solitude

import Effectful.Optics ( (%=) )

import Yaifl.Core.Actions.Action ( WorldActions, whenPlayBegins )
import Yaifl.Core.Actions.Activity ( ActivityCollection )
import Yaifl.Core.Entity ( Store, Entity )
import Yaifl.Core.Metadata ( Metadata )
import Yaifl.Core.Objects.RoomData ( RoomData )
import Yaifl.Core.Objects.ThingData ( ThingData )
import Yaifl.Core.Rulebooks.Rule ( Rule )
import Yaifl.Core.Rulebooks.Rulebook ( addRuleLast )
import Yaifl.Core.Say ( Has(..), MessageBuffer )
import Yaifl.Core.WorldModel ( WMValues, WorldModel )
import Yaifl.Core.Object (Object)

data World (wm :: WorldModel) = World
  { metadata :: Metadata
  , stores :: WorldStores wm
  , actions :: WorldActions wm
  , activities :: ActivityCollection wm
  , messageBuffer :: MessageBuffer
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