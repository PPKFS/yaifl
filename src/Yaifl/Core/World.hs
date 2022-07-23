-- ~\~ language=Haskell filename=src/Yaifl/Core/World.hs
-- ~\~ begin <<lit/worldmodel/state.md|src/Yaifl/Core/World.hs>>[0] project://lit/worldmodel/state.md:9

{-# LANGUAGE TemplateHaskell #-}
module Yaifl.Core.World where

import Yaifl.Core.Common ( Entity, Metadata, Store, WMValues, WorldModel )
import Yaifl.Core.Say ( Has(..), MessageBuffer )
import Yaifl.Core.Actions.Action ( WorldActions, whenPlayBegins )
import Yaifl.Core.Objects.Dynamic ( AbstractRoom, AbstractThing )
import Cleff.State ( State )
import Yaifl.Core.Rulebooks.Rulebook ( addRuleLast )
import Yaifl.Core.Rulebooks.Rule ( Rule )
import Yaifl.Core.Actions.Activity ( ActivityCollection )
import Yaifl.Core.Logger

data World (wm :: WorldModel) = World
  { _worldMetadata :: Metadata
  , _worldStores :: WorldStores wm
  , _worldActions :: WorldActions wm
  , _worldActivities :: ActivityCollection wm
  , _messageBuffer :: MessageBuffer
  , _worldLogs :: LogBuffer
  }

-- ~\~ begin <<lit/worldmodel/state.md|world-stores>>[0] project://lit/worldmodel/state.md:115
data WorldStores (wm :: WorldModel) = WorldStores
  { _entityCounter :: (Entity, Entity)
  , _things :: Store (AbstractThing wm)
  , _rooms :: Store (AbstractRoom wm)
  , _values :: Map Text (WMValues wm)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  }
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/state.md|world-actions>>[0] project://lit/worldmodel/state.md:129
makeLenses ''World
makeLenses ''WorldModel
makeLenses ''WorldStores

instance Has (World wm) MessageBuffer where
  buf = messageBuffer

instance Has (World wm) LogBuffer where
  buf = worldLogs
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/state.md|world-other>>[0] project://lit/worldmodel/state.md:155

addWhenPlayBegins ::
  State (WorldActions wm) :> es
  => Rule wm () Bool
  -> Eff es ()
addWhenPlayBegins r = whenPlayBegins %= addRuleLast r
-- ~\~ end
-- ~\~ end
