# Entities, Stores and Objects

Finally, we can start actually seeing some Haskell. This chapter contains the 3 most basic building blocks:

- **Entities** - An ID that allows us to circumvent immutability by breaking apart references between objects.
- **Stores** - `Map`s of `Entity`s to various objects.
- **Objects** - Game objects, spanning from the physical (keys, doors, people) to the intangible (rooms, etc).

Between these 3, we can construct (with some difficulty) a world model that can represent some domain of IF,
but not one that can be interacted with.

### Brief comments on some other systems

There's certainly a couple of other ways to approach this idea (that aren't "just write in OOP"), which I figured were worth mentioning.

- **Entity-component systems (ECS)** - these are certainly doable in Haskell - `ecstasy` and `apecs` come to mind, and to some degree we are designing an ECS that cannot change at runtime - but it's not great because the semantic approach is that you want to iterate over sets of *components* and less so care about what they are attached to -- this is the exact opposite to a text adventure (where you rarely care about *all* doors or vehicles or whatever, rather just a specific one).
- **Ad-hoc, extensible records** - By which I mean some sort of system where there is no distinction between discrete classes of kinds, but rather everything is an anonymous record (plus/minus some type-wizardry) with named fields. This would probably be a good option, but I felt it un-idiomatic. 


### Aims of the Entity/Store/Model structure

- Everything should be Haskell-ish. Whilst the use of `lens` (or in this case, `optics`) is almost a given for a program that works so heavily on nested data structure modification, and it being a "game" implies the existence of some monolithic state, I would like to be able to work in pure functions where possible.  
- Everything should be extensible with minimal effort. Any more direct way to include such semantics as "A thing can be sticky or not sticky. A thing is usually sticky" as modifying the very concept of a `Thing` is getting into dynamic typing territory. Rather, by utilising smart constructors, we can write simple wrappers of `makeStickyThing` that use `ObjectSpecifics` that look like `(Stickiness, a)` and *voila*, we have redefined every `Thing` in the program. NB: this does mean there needs to be some care taken when we automatically generate objects and rooms (the player, the void).
- Type safety. We're in Haskell, so act like it! It makes no sense to call `move` on a `Room` and a `Scenery`, for instance.

## Entities

And the big reveal of the lynchpin of the entire library is...

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
```

We also then reserve a few IDs for the 'default' objects which we never want to see at runtime, but need at construction
time to avoid unnecessary `Maybe`s.
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
`Store Entity` for relations and things like that.

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

## Objects

And now we turn to the final of the foundational parts of the world model, the `Object`. Whilst we don't go quite as far as
Inform does, where *everything* is an object of some kind (including directions, or if you wanted to extend the system with ideas such as people having knowledge of something), we still consider most things in a game to be an `Object` of some kind. These can be split into two categories; `Thing`s (physical, interactable, objects) and `Room`s (spaces to be moved between). Each of these can be further divided into more specific instances, but it is significantly simpler to deal with everything being either a realisable object or a space, and treat the very few intangible objects as their own specific thing (for example, directions). The obvious downside of this is that we have to treat directions specially -- but since when has anyone written a piece of IF where they need to invent new directions *at runtime*? 

```haskell id=obj-definition
data Object wm objData = Object
  { _objName :: !Text
  , _objDescription :: !Text
  , _objID :: !Entity
  , _objType :: !ObjType
  , _objCreationTime :: !Timestamp
  , _objSpecifics :: !(Either ObjectSpecifics (WMObjSpecifics wm))
  , _objData :: !objData
  } deriving stock (Generic)

deriving stock instance (Show (WMObjSpecifics wm), Show d) => Show (Object wm d)
deriving stock instance (Read (WMObjSpecifics wm), Read d) => Read (Object wm d)
```

So there's a handful of things here:  

- `Object`s are parameterised by two type parameters; `wm :: WorldModel`, which we use to define the extensions of `ObjectSpecifics` (to be discussed below), and `objData`, which will always be one of 3 types:
  - `ThingData wm`, and we have a `Thing`;
  - `RoomData wm`, and we have a `Room`;
  - `Either (ThingData wm) (RoomData wm)`, and we have no idea which so we call it `AnyObject`.
- Yes, everything is prefixed with an underscore because we use a *lot* of lens TH generation.
- This is the first of many standalone deriving instances. Will we ever need `Read (Object wm d)`? Probably not, but still.
- `ObjType` may look strange given how much we are trying to avoid remaking OO here, but this use of an object "type" is more of a tag system:

```haskell id=obj-type
newtype ObjType = ObjType
  { unObjType :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsList, IsString, Monoid, Semigroup)
```
`ObjType`s make a DAG that approximates inheritance in name only. For instance, we may wish to check that an object *is* a supporter for printing locale descriptions (where we want to say "on the table" rather than "in the table"). We aren't being polymorphic and imitating v-tables, or deriving properties automatically - we just note that "supporter" is a valid object type and e.g. a "display cabinet" is also a kind of supporter, and a "glass fronted display cabinet" is a kind of display cabinet and we can infer the transitive property.

And at last, we can talk about `Thing`, `Room`, and `AnyObject`:

```haskell id=thing-room-anyobject
-- | Some of the (very rare) type aliases, just to make it easier to describe `Thing`s and `Room`s.
type Thing wm = Object wm ThingData
type Room wm = Object wm (RoomData wm)
type AnyObject wm = Object wm (Either ThingData (RoomData wm))
```

### Some other useful instances 

Obviously we can get an `ID` out of an `Object`:

```haskell id=obj-hasid
instance HasID (Object wm d) where
  getID = _objID
```

The other obvious missing one is `Eq`; if we automatically derive `Eq`, we can only compare objects with the same `objData` type -- so no comparison at all of `Room`s and `Thing`s (even if that's always `False`) or even `Thing`s and `AnyObject`s (which may be true). So we write our own that is slightly more lenient on the types, and `Ord` too even though it makes no sense.

```haskell id=obj-eq
objectEquals :: 
  Object wm d
  -> Object wm d'
  -> Bool
objectEquals = (. _objID) . (==) . _objID

instance Eq (Object wm d) where
  (==) = objectEquals

-- | Maybe I'll need this instance for something or other? 
instance Ord (Object wm d) where
  compare = (. _objID) . compare . _objID
```

Another fairly useful set of instances that don't make a huge amount of sense for anything other than ease of use: `Functor`, `Foldable`, and `Traversable`. All 3 work over `objData`, so the primary use is to go back-and-forth from `AnyObject`.

```haskell id=obj-functor

instance Functor (Object wm) where
  fmap :: 
    (a -> b)
    -> Object wm a
    -> Object wm b
  fmap f = objData %~ f

instance Foldable (Object wm) where
  foldMap :: 
    (a -> m)
    -> Object wm a 
    -> m
  foldMap f = f . _objData

instance Traversable (Object wm) where
  traverse :: 
    Applicative f 
    => (a -> f b) 
    -> Object wm a 
    -> f (Object wm b)
  traverse f o = (\v -> o {_objData = v}) <$> f (_objData o)
```

### Lenses and Prisms

For the most part I'm omitting fluff like `makeLenses ''Object` (if you're really interested, they will be in the full `noweb` file in the `Reference` section). However there are two very useful prisms we can define. Originally, these were their own typeclass called `CanBeAny` - because they looked like 

```haskell
toAny :: a -> b
fromAny :: b -> Maybe a
```

but then I realised these were just `preview` and `review` respectively. 

```haskell obj-prisms
_Room :: Prism' (AnyObject wm) (Room wm)
_Room = prism' (fmap Right) (traverse rightToMaybe)

_Thing :: Prism' (AnyObject wm) (Thing wm)
_Thing = prism' (fmap Left) (traverse leftToMaybe)
```

Though I keep the class around regardless, because `toAny o` makes more semantic sense than `review _Thing`.

```haskell id=can-be-any
class CanBeAny wm o where
  toAny :: o -> AnyObject wm
  fromAny :: AnyObject wm -> Maybe o

instance CanBeAny wm (Room wm) where
  toAny = review _Room
  fromAny = preview _Room

instance CanBeAny wm (Thing wm) where
  toAny = review _Thing
  fromAny = preview _Thing

instance CanBeAny wm (AnyObject wm) where
  toAny = id
  fromAny = Just
```

## ObjectLike

Something that appears often in the code but isn't really its own thing is `ObjectLike`; something that *can* be evaluated to an `Object` in a context `m`, but isn't necessarily an object itself. `NoMissingObjects` and `MonadWorld` will be explained more in the [Effects](foundations/effects.md) section, but for now they can be read as "block where failed `Entity` lookups are fail conditions" and "block where we can query the existence of an object".

```haskell id=objectlike
  
class HasID o => ObjectLike wm o where
  getRoom :: (NoMissingObjects m, MonadWorld wm m) => o -> m (Room wm)
  default getRoom :: (NoMissingObjects m) => o -> m (Room wm)
  getRoom o = throwError $ MissingObject "Called getRoom on an object with no instance."  (getID o)
  
  getThing :: (NoMissingObjects m, MonadWorld wm m) => o -> m (Thing wm)
  default getThing :: (NoMissingObjects m) => o -> m (Thing wm)
  getThing o = throwError $ MissingObject "Called getThing on an object with no instance."  (getID o)

instance ObjectLike wm (Thing wm) where
  getThing = pure

instance ObjectLike wm (Room wm) where
  getRoom = pure

instance ObjectLike wm (AnyObject wm) where
  getThing t = liftEither
    (maybeToRight (MissingObject ("Tried to get a thing from " <> show (_objID t) <> " but it was a room.") (getID t))
      (preview _Thing t))
  getRoom t = liftEither
    (maybeToRight (MissingObject ("Tried to get a room from " <> show (_objID t) <> " but it was a thing.") (getID t))
      (preview _Room t))
```