{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.World where

import Solitude

import Effectful.Optics ( (%=) )

import Yaifl.Core.Actions.Action ( WorldActions, whenPlayBegins )
import Yaifl.Core.Actions.Activity ( ActivityCollection )
import Yaifl.Core.Entity ( Store, Entity )
import Yaifl.Core.Metadata ( Metadata )
import Yaifl.Core.Objects.Dynamic (AbstractObject)
import Yaifl.Core.Objects.RoomData ( RoomData )
import Yaifl.Core.Objects.ThingData ( ThingData )
import Yaifl.Core.Rulebooks.Rule ( Rule )
import Yaifl.Core.Rulebooks.Rulebook ( addRuleLast )
import Yaifl.Core.Say ( Has(..), MessageBuffer )
import Yaifl.Core.WorldModel ( WMValues, WorldModel )

data World (wm :: WorldModel) = World
  { _worldMetadata :: Metadata
  , _worldStores :: WorldStores wm
  , _worldActions :: WorldActions wm
  , _worldActivities :: ActivityCollection wm
  , _messageBuffer :: MessageBuffer
  }

data WorldStores (wm :: WorldModel) = WorldStores
  { _entityCounter :: (Entity, Entity)
  , _things :: Store (AbstractObject wm ThingData)
  , _rooms :: Store (AbstractObject wm (RoomData wm))
  , _values :: Map Text (WMValues wm)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  }

makeLenses ''World
makeLenses ''WorldModel
makeLenses ''WorldStores

instance Has (World wm) MessageBuffer where
  buf = messageBuffer

addWhenPlayBegins ::
  State (WorldActions wm) :> es
  => Rule wm () Bool
  -> Eff es ()
addWhenPlayBegins r = whenPlayBegins %= addRuleLast r