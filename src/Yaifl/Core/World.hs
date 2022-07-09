-- ~\~ language=Haskell filename=src/Yaifl/Core/World.hs
-- ~\~ begin <<lit/worldmodel/state.md|src/Yaifl/Core/World.hs>>[0] project://lit/worldmodel/state.md:9

{-# LANGUAGE TemplateHaskell #-}
module Yaifl.Core.World where
import Solitude
import Yaifl.Core.Common

import Yaifl.Core.Say
--import Yaifl.Core.Rulebooks.Rulebook
--import Yaifl.Core.Activities.Activity
import Yaifl.Core.Actions.Action
import Yaifl.Core.Objects.Dynamic
import Cleff.State
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Create
import Yaifl.Core.Rulebooks.Rulebook ( addRuleLast )
import Yaifl.Core.Rulebooks.Rule

data World (wm :: WorldModel) = World
  { _worldMetadata :: Metadata wm
  , _worldStores :: WorldStores wm
  , _worldActions :: WorldActions wm
  , _messageBuffer :: MessageBuffer
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
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/state.md|world-other>>[0] project://lit/worldmodel/state.md:155

addWhenPlayBegins ::
  State (WorldActions wm) :> es
  => Rule wm () Bool
  -> Eff es ()
addWhenPlayBegins r = whenPlayBegins %= addRuleLast r

-- | Turn an `AbstractObject` into a regular `Object` and update the cache if needed.
reifyObject ::
  State (Metadata wm) :> es
  => (AbstractObject wm d -> Eff es ())
  -> AbstractObject wm d
  -> Eff es (Object wm d)
reifyObject _ (StaticObject v) = return v
reifyObject setFunc (DynamicObject ts) = do
  let co = _tsCachedObject ts
  now <- getGlobalTime
  if
    _tsCacheStamp ts == now
  then
    return co
  else
    do
      -- update the object
      updatedObj <- runObjectUpdate (_tsUpdateFunc ts) co
      t <- getGlobalTime
      setFunc (DynamicObject $ TimestampedObject updatedObj t (_tsUpdateFunc ts))
      return updatedObj

reifyRoom :: 
  State (Metadata wm) :> es
  => (ObjectCreation wm :> es)
  => AbstractRoom wm
  -> Eff es (Room wm)
reifyRoom = reifyObject addAbstractRoom

reifyThing :: 
  State (Metadata wm) :> es
  => (ObjectCreation wm :> es)
  => AbstractThing wm
  -> Eff es (Thing wm)
reifyThing = reifyObject addAbstractThing




{-
tickGlobalTime :: 
  MonadWorld wm m
  => Bool
  -> m ()
--I have no idea what my plans were for this flag.
tickGlobalTime False = dirtyTime .= True
tickGlobalTime True = do
  dirtyTime .= False
  _ <- globalTime <%= (+1)
  pass
  -- debug (bformat ("Dong. The time is now " %! int %! ".") r)


-}
-- ~\~ end
-- ~\~ end
