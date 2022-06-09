# Objects

And now we turn to the final of the foundational parts of the world model, the `Object`. Whilst we don't go quite as far as
Inform does, where *everything* is an object of some kind (including directions, or if you wanted to extend the system with ideas such as people having knowledge of something), we still consider most things in a game to be an `Object` of some kind. These can be split into two categories; `Thing`s (physical, interactable, objects) and `Room`s (spaces to be moved between). 

Each of these can be further divided into more specific instances, but it is significantly simpler to deal with everything being either a realisable object or a space, and treat the very few intangible objects as their own specific thing (for example, directions). The obvious downside of this is that we have to treat directions specially -- but since when has anyone written a piece of IF where they need to invent new directions *at runtime*? 

First we have the overview of the module.

```haskell file=src/Yaifl/Objects/Object.hs
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Objects.Object ( 
  -- * Types
  ObjType(..)
  , Object(..)
  , Thing
  , Room
  , AnyObject
  -- * Object Helpers
  , objectEquals
  -- * Lenses
  , objName, objDescription, objID, objType
  , objCreationTime, objSpecifics, objData
  -- * Prisms
  , _Room, _Thing ) where

import Solitude
import Yaifl.Common ( WMObjSpecifics, Timestamp, HasID(..), Entity )
import Yaifl.Objects.ObjectData ( RoomData, ThingData )
import Yaifl.Objects.Specifics ( ObjectSpecifics )

<<obj-type>>
<<thing-room-anyobject>>
<<obj-definition>>
<<obj-hasid>>
<<obj-eq>>
makeLenses ''Object
<<obj-functor>>

<<obj-prisms>>
<<can-be-any>>
```

And the object type itself:

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
`ThingData` and `ObjectData` are covered in [the next section](data.md)
- Yes, everything is prefixed with an underscore because we use a *lot* of lens TH generation.
- This is the first of many standalone deriving instances. Will we ever need `Read (Object wm d)`? Probably not, but still.

## Some useful instances 

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

## Object Types

`ObjType` may look strange given how much we are trying to avoid remaking OO here, but this use of an object "type" is more of a tag system:

```haskell id=obj-type
newtype ObjType = ObjType
  { unObjType :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsList, IsString, Monoid, Semigroup)
```
`ObjType`s make a DAG that approximates inheritance in name only. For instance, we may wish to check that an object *is* a supporter for printing locale descriptions (where we want to say "on the table" rather than "in the table"). We aren't being polymorphic and imitating v-tables, or deriving properties automatically - we just note that "supporter" is a valid object type and e.g. a "display cabinet" is also a kind of supporter, and a "glass fronted display cabinet" is a kind of display cabinet and we can infer the transitive property.

---

## Things, Rooms, AnyObjects

And at last, we can talk about `Thing`, `Room`, and `AnyObject`:

```haskell id=thing-room-anyobject
type Thing wm = Object wm ThingData
type Room wm = Object wm (RoomData wm)
type AnyObject wm = Object wm (Either ThingData (RoomData wm))
```

### Lenses and Prisms

For the most part I'm omitting fluff like `makeLenses ''Object` (if you're really interested, they will be in the full `noweb` file in the `Reference` section). However there are two very useful prisms we can define. Originally, these were their own typeclass called `CanBeAny` - because they looked like 

```haskell
toAny :: a -> b
fromAny :: b -> Maybe a
```

but then I realised these were just `preview` and `review` respectively. There is an equivalent for `AbstractObject` as well, though whether it's useful isn't yet decided.

```haskell id=obj-prisms
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
