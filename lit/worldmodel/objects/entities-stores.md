# Entities and Stores

By decentralising references between objects and instead storing some kind of ID and some kind of `Map ID Object`, we can avoid mutable state. Hooray!
## Entities

```haskell id=entity-def
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)
```

A `newtype` wrapper around  `Int`. Yup, that's about it. One nice feature is that we can, under the assumption
that nobody does something strange (like turning a `Thing` into a `Room`), determine whether a given `Entity` 
refers to a `Thing` or a `Room` by whether we generated the ID by counting up or down:

```haskell id=thing-or-room
isThing ::
  (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom ::
  (HasID a)
  => a
  -> Bool
isRoom = not . isThing
```

It's also nice to have a way to always get an `Entity` from a construct:

```haskell id=has-id
class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id

instance Display Entity where
  display (Entity i) = "ID: " <> show i

```

We also then reserve a few IDs for the 'default' objects which we never want to see at runtime, but need at construction time to avoid unnecessary `Maybe`s.

```haskell id=base-ids
defaultVoidID :: Entity
defaultVoidID = Entity (-1)

defaultNothingID :: Entity
defaultNothingID = Entity 0

defaultPlayerID :: Entity
defaultPlayerID = Entity 1
```
## Stores

A `Store` is a map from `Entity`s to `a`s. Usually this is some flavour of `Object wm d`, but we can also use
`Store (Entity, Payload)` for relations and things like that. Of course, since I've refactored the direct link of a `Map`-based store to a specific interpretation of the world's effects, this seems slightly out of place. But it's fairly obvious as the go-to implementation so it may as well stay here.

```haskell id=store-def
-- import qualified Data.EnumMap.Strict as EM
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Ord, Read)

emptyStore :: Store a
emptyStore = Store EM.empty
```

### EnumMap and Optics
`EnumMap` (and its sibling `EnumSet`) are nice convenient `newtype` wrappers around `IntMap`, but they're not
quite cut out for a) *further* `newtype` wrappers around *them*, and b) the instances for nice `Lens`/`Optics` things.

First let's define our own [`alterF`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Strict.html#v:alterF) for `EnumMap` and then *another* for a `newtype` wrapper...

```haskell id=alter-store
alterEMF :: 
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a 
  -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

alterNewtypeEMF :: 
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> (nt -> EM.EnumMap k a)
  -> (EM.EnumMap k a -> nt)
  -> nt
  -> f nt
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)
```

Which we can now use for a nice and tidy `At` instance.

```haskell id=store-at
instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store
```

Finally, we choose the obvious instantiations for the associated index types.

```haskell id=store-instances
type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)
```

Which means we can now write use our lenses as `someStore ^? at someEntity` rather than `someStore ^? coercedTo @(EnumMap Entity a) % to unStore % at someEntity`, or some other verbose beast.
