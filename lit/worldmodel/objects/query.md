# Object Querying Effects

This is an excellent example of how smaller effects mean less boilerplate over the equivalent mtl solution. I think. At least it certainly avoid the O(n^2) instances that come from having a `ObjectRead`, `ObjectWrite`, `ObjectQuery`, etc set of constraints.

```haskell file=src/Yaifl/Objects/Query.hs
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Query
  ( -- * Types
  ObjectLike(..)
  , MissingObject(..)
  , ObjectQuery(..)
  -- * Missing Objects
  , withoutMissingObjects 
  , failHorriblyIfMissing
  , handleMissingObject
  , NoMissingObjects
  -- * Get
  , lookupThing
  , lookupRoom
  , getObject
  , getThingMaybe
  , getRoomMaybe
  , asThingOrRoom
  -- * Modify
  , modifyObject
  , modifyThing
  , modifyRoom
  -- * Set
  , setThing
  , setRoom
  ) where

import Cleff.Error ( Error, fromEither, runError, throwError )
import Cleff.State ( State )
import qualified Data.Text.Lazy.Builder as TLB

import Solitude

import Yaifl.Common ( Metadata, WorldModel, HasID(..), Entity, isThing )
import Yaifl.Logger ( Log, err )
import Yaifl.Objects.Object ( _Room, _Thing, AnyObject, Object(_objID), Room, Thing )

<<missing-object>>
<<handle-missing-objects>>
<<object-query-effect>>
<<objectlike>>
<<objectlike-instances>>
<<get-objects>>
<<modify-objects>>
```

## Missing Objects

It's much easier to use some kind of `Error` effect to avoid wrapping endless amounts of object querying in `Maybe`. We start with a simple payload that holds whatever object was looked for (and failed to find), and some contextual error message.

```haskell id=missing-object
data MissingObject = MissingObject 
  { _moExpected :: Text
  , _moEntity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeLenses ''MissingObject
```

```haskell id=handle-missing-objects
withoutMissingObjects :: 
  (HasCallStack => Eff (Error MissingObject ': es) a) -- ^ the block
  -> (HasCallStack => MissingObject -> Eff es a)  -- ^ the handler
  -> Eff es a
withoutMissingObjects f def = do
  r <- runError f
  case r of
    Left err' -> def err'
    Right x -> return x

handleMissingObject :: 
  HasCallStack
  => Log :> es
  => TLB.Builder 
  -> Eff es a 
  -> MissingObject
  -> Eff es a
handleMissingObject msg def (MissingObject t o) = do
  err (msg <> bformat (stext %! "; Object ID: " %! stext) t (show o))
  def

failHorriblyIfMissing ::
  Log :> es
  => (HasCallStack => Eff (Error MissingObject ': es) a)
  -> Eff es a
failHorriblyIfMissing f = withoutMissingObjects f (\(MissingObject t o) -> do
  let msg = "Failing horribly and erroring out because we can't recover"
      emsg = msg <> bformat (stext %! "; Object ID: " %! stext) t (show o)
  err emsg
  error $ show emsg)
```

## Object querying

And now we have the effects themselves. A write-only effect wouldn't be particularly useful, and similarly there's not a great use-case for thing-only or room-only. We return `Either` in the `lookup` functions because we have multiple possible fail cases (when we are reifying dynamic objects as well as the expected failed lookup). The `Either` is consumed by higher-level functions, so we probably never use `lookup` directly.

```haskell id=object-query-effect
data ObjectQuery (wm :: WorldModel) :: Effect where
  LookupThing :: HasID o => o -> ObjectQuery wm m (Either Text (Thing wm))
  LookupRoom :: HasID o => o -> ObjectQuery wm m (Either Text (Room wm))
  SetRoom :: Room wm -> ObjectQuery wm m ()
  SetThing :: Thing wm -> ObjectQuery wm m ()

makeEffect ''ObjectQuery

type NoMissingObjects wm es = (Error MissingObject :> es, ObjectQuery wm :> es, State (Metadata wm) :> es) 
```
## ObjectLike

A slight detour to things which are *almost* objects but require looking up extra information in the effect stack. This is slightly more restrictive than `HasID`.

```haskell id=objectlike
class HasID o => ObjectLike wm o where
  getRoom :: NoMissingObjects wm es => o -> Eff es (Room wm)
  default getRoom :: NoMissingObjects wm es => o -> Eff es (Room wm)
  getRoom o = throwError $ MissingObject "Called getRoom on an object with no instance."  (getID o)

  getThing :: NoMissingObjects wm es => o -> Eff es (Thing wm)
  default getThing :: (NoMissingObjects wm es) => o -> Eff es (Thing wm)
  getThing o = throwError $ MissingObject "Called getThing on an object with no instance."  (getID o)
```

And we have the obvious instances for `Object`s themselves, to eliminate the `Either` out of an `AnyObject`, and the sneakily most important instance that wraps `lookup` with removing the fail case.

```haskell id=objectlike-instances
instance ObjectLike wm (Thing wm) where
  getThing = pure

instance ObjectLike wm (Room wm) where
  getRoom = pure

instance ObjectLike wm (AnyObject wm) where
  getThing t = fromEither
    (maybeToRight (MissingObject ("Tried to get a thing from " <> show (_objID t) <> " but it was a room.") (getID t))
      (preview _Thing t))
  getRoom t = fromEither
    (maybeToRight (MissingObject ("Tried to get a room from " <> show (_objID t) <> " but it was a thing.") (getID t))
      (preview _Room t))

instance ObjectLike wm Entity where
  getRoom e = lookupRoom e >>= either (throwError . flip MissingObject e) return
  getThing e = lookupThing e >>= either (throwError . flip MissingObject e) return
```

Most of the `get` functionality is in `ObjectLike`, but we have a couple of useful functions here:

- if we don't know the type of an object (and it's irrelevant enough to not bother querying the ID type at the call site), we can just get `AnyObject`.  
- sometimes we don't want to throw an error or deal with `NoMissingObjects` blocks, so we can wrap it back into a `Maybe`.  
- Finally we can do coproduct elimination by handling both cases of an `Object`.

```haskell id=get-objects
getObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es (AnyObject wm)
getObject e = if isThing e
  then (review _Thing <$> getThing e)
  else (review _Room <$> getRoom e)

getThingMaybe :: 
  ObjectQuery wm :> es
  => State (Metadata wm) :> es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Thing wm))
getThingMaybe o = withoutMissingObjects (getThing o <&> Just) (const (return Nothing))

getRoomMaybe ::
  ObjectQuery wm :> es
  => State (Metadata wm) :> es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Room wm))
getRoomMaybe o = withoutMissingObjects (getRoom o <&> Just) (const (return Nothing))

asThingOrRoom :: 
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> a)
  -> (Room wm -> a)
  -> Eff es a
asThingOrRoom o tf rf =
  if isThing o
  then tf <$> getThing o
  else rf <$> getRoom o
```

## Modifying Objects

For modifying, we have a helper function that is basically a verbose and law-breaking lens and then modifying specific objects is simply a curried version. The awkward case is modifying an `AnyObject` because technically the object *could* switch from `Thing` to `Room` in the middle but we trust that not to happen.

```haskell id=modify-objects
modifyObjectFrom :: 
  (o -> Eff es (Object wm any))
  -> (Object wm any -> Eff es ())
  -> o
  -> (Object wm any -> Object wm any)
  -> Eff es ()
modifyObjectFrom g s o u = do
  obj <- g o
  s (u obj)
  pass

modifyThing :: 
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> Thing wm)
  -> Eff es ()
modifyThing = modifyObjectFrom getThing setThing 

modifyRoom ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Room wm -> Room wm)
  -> Eff es ()
modifyRoom = modifyObjectFrom getRoom setRoom

modifyObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> Eff es ()
modifyObject e s = 
  if isThing e
  then modifyThing e (anyModifyToThing s)
  else modifyRoom e (anyModifyToRoom s)

anyModifyToThing :: 
  (AnyObject s -> AnyObject s)
  -> (Thing s -> Thing s)
anyModifyToThing f t = fromMaybe t (preview _Thing $ f (review _Thing t))

anyModifyToRoom :: 
  (AnyObject s -> AnyObject s)
  -> (Room s -> Room s)
anyModifyToRoom f t = fromMaybe t (preview _Room $ f (review _Room t))
```
