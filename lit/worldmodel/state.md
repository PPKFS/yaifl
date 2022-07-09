# The World State

This is the monolithic core of the library. It can be chunked into a few main pieces, each of which is handled by a separate effect handler to keep our separation of concerns as modular as possible (for instance, an object should not be able to run an `Action` when printing out some text!).

We put the message buffer as part of the state even though we are in `IO` because it allows us to directly consume the output (e.g. for testing).


```haskell file=src/Yaifl/Core/World.hs

{-# LANGUAGE TemplateHaskell #-}
module Yaifl.Core.World where
import Solitude
import Yaifl.Core.Common

import Yaifl.Core.Say
--import Yaifl.Core.Rulebooks.Rulebook
--import Yaifl.Core.Activities.Activity
import Yaifl.Core.Actions.Action
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Objects.Query
import Cleff.State
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Create
import Display
import Yaifl.Core.Rulebooks.Rulebook ( Rulebook, addRuleLast, UnverifiedArgs )
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Actions.Action

data World (wm :: WorldModel) = World
  { _worldMetadata :: Metadata wm
  , _worldStores :: WorldStores wm
  , _worldActions :: WorldActions wm
  , _messageBuffer :: MessageBuffer
  }

<<world-stores>>
<<world-actions>>
<<world-other>>
```
---
## Metadata

These fields are for information we need to keep around but don't have complex dependencies -- so we can define the metadata in `Yaifl.Common` and therefore break the import cycle of individual modules <-> `Yaifl.World` we have with a monolithic state record, but without having to write the boilerplate of an effect that exposes each of these fields individually.

```haskell id=world-metadata

data Metadata (wm :: WorldModel) = Metadata
  { _title :: Text
  , _roomDescriptions :: RoomDescriptions
  , _dirtyTime :: Bool
  , _globalTime :: Timestamp
  , _darknessWitnessed :: Bool
  , _currentPlayer :: Entity
  , _currentStage :: CurrentStage
  , _previousRoom :: Entity
  , _firstRoom :: Entity
  -- more to come I guess
  }

data CurrentStage = Construction | Verification | Runtime
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

makeLenses ''Metadata

getGlobalTime :: 
  State (Metadata wm) :> es 
  => Eff es Timestamp
getGlobalTime = use globalTime

tickGlobalTime ::
  State (Metadata wm) :> es 
  => Bool
  -> Eff es ()
tickGlobalTime _ = pass

setTitle :: 
  State (Metadata wm) :> es 
  => Text -- ^ New title.
  -> Eff es ()
setTitle = (title .=)

whenConstructingM :: 
  State (Metadata wm) :> es
  => Eff es Bool 
  -> Eff es () 
  -> Eff es ()
whenConstructingM cond = 
  whenM (andM [do
    cs <- use currentStage
    return $ cs == Construction, cond])

```

```haskell id=world-staging
data CurrentStage = Construction | Verification | Runtime
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

```
### Room Descriptions

Lifted directly from Inform; this sets whether to always print room descriptions (No..) even if the room is visited, to only print them on the first entry (Sometimes..), or never.

```haskell id=room-descriptions
data RoomDescriptions = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions 
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)
```

## World Stores

The lookup tables for various objects, values, etc in the game. This is probably the most important part of the `World` state.

```haskell id=world-stores
data WorldStores (wm :: WorldModel) = WorldStores
  { _entityCounter :: (Entity, Entity)
  , _things :: Store (AbstractThing wm)
  , _rooms :: Store (AbstractRoom wm)
  , _values :: Map Text (WMValues wm)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  }
```

## World Actions

These are the dynamic parts that run things. This is the in-world actions, the standard activities (because user-defined activities can be done separately and don't need a lookup), and the two standalone rulebooks. Again, user-defined rulebooks act the same as activities and don't need to be stored around in the state.

```haskell id=world-actions


makeLenses ''World
makeLenses ''WorldModel
makeLenses ''WorldStores

```

### Timestamp Caching

It is up to functions which might do some more complex processing (e.g. `move`) to update the time.

```haskell id=timestamp

newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

```

---

# Other

```haskell id=world-other

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

addBaseActions :: 
  HasLookingProperties wm
  => World wm
  -> World wm
addBaseActions = foldr (.) id [
    addAction lookingActionImpl
  , addAction goingActionImpl
  ]
-}
```
