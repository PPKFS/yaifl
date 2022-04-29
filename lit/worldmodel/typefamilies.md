# WorldModel and WMx Type Families

## The problem

One thing that has repeatedly annoyed me when trying to hack around on `yaifl` is the desire to be as general as possible, but whilst doing my best to avoid writing imperative code but in Haskell. One of these major headscratching problems was how to have something akin to extensible records but in a more Haskell-y way. For instance, take object types. We can provide a standard library of `Thing`, `Room`, `Door`, `Person`, `Vehicle`, etc. just fine; but what if we want to have a `Gate`? In an OO- language this is fine - just inherit from `Door`. In Haskell we can approximate this in the same way `Parsec` deals with its error types:

```haskell

data ObjectType a = ThingType Thing | DoorType Door | RoomType Room ... | Other a
```

And we are then open (as long as the user supplied `a` satisfies constraints that we impose, such as `Show` or `Eq`) to extensionality but closed at compile time. This has the drawback that if you want this open-closed hierarchy on *many* types of data, you have a monolithic state that
looks something like `MonolithicState a b c d e f g`:

- We want horizontally extendable object types, so we can start with `MonolithicWorld objType`. All cool.
- Now we want directions; whilst the compass points are probably fine for nearly every game, sometimes you might want to have turnwise or widdershins. Now you have `MonolithicWorld objType directionType`. 
- What about arbitrary data variables you want to keep track of during the game? Now you've got `MonolithicWorld objType directionType variableRecord`. 
- And so on.

Chances are that most of those are going to be `()` - you don't want extra directions, or you don't define a special kind of door - but it's a huge pain to write out multiple times for each function signature.

We could work in an `mtl`-style way of component instances and typeclasses:

```haskell
data MonolithicWorld s = MonolithicWorld
  { ...
  , worldData :: s
  }

data WorldData a b c d e f g = WorldData
  { things :: Store (Object a)
  , directions :: Store (Direction b)
  ...
  -- and so on
  }

class HasThings s a where
  things :: Lens' (MonolithicWorld s) (Store (Object a))

class HasDirections s b where
  directions :: Lens' (MonolithicWorld s) (Store (Direction b))
```

But this quickly ran into the same problem; a function that dealt with both `Direction`s and `Object`s still needs both `a` and `b` to be parametric, and these constraints bubbled up to the top...plus, I'd often get `ambiguous type variable a1` issues.

What if there was a way to do this with some type-level nonsense?

## The WorldModel type families

Behold, a whole bunch of random `Type`s!

```haskell id=world-model
data WorldModel = WorldModel Type Type Type Type
```

By using `DataKinds`, we can promote this to the type level. We can now start making types that look like

```haskell
data SomeExtraObjTypes = GateType Gate
type Score = Int
type AWorldModel = 'WorldModel SomeExtraObjTypes () () Score
```

which is great; we can parameterise everything by `(wm :: WorldModel)`, and now we have only one type variable instead of 4 (or more)! But record field accessors don't work at the type-level (F in chat), so we need to write a little boilerplate:

```haskell id=world-model-families
type family WMObjSpecifics (wm :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir o v) = objSpec

type family WMDirections (wm :: WorldModel) :: Type where
  WMDirections ('WorldModel objSpec dir o v) = dir 

type family WMValues (wm :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir o v) = o
```

How does this work? Without doing a terrible job of massacring an explanation of how type families work, we can view these as very basic dependent types; given some type instantiation of `wm :: WorldModel`, we have an associated type `WMObjSpecifics` that is defined by the first member of that type. Now, rather than ever referring to the `objSpec` we can refer to `WMObjSpecifics wm`. Everything is unified and there's no unnecessary typeclass baggage, rejoice!

Well, there is one slight issue - this breaks GHC's `deriving` machinery. Types that contain a `WMFoo wm` have to instead use quantified instance derivations; for instance we may need to define an `Ord` instance like

```haskell
deriving stock instance (Ord (WMDirections wm), Ord (WMObjSpecifics wm)) => Ord (FooBar wm)
```

which gets minorly annoying. But thanks to `ConstraintKinds` we can write some tidy helper types.

```haskell id=world-model-constraints
type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirections wm))
type WMShow wm = WMConstr Show wm
type WMRead wm = WMConstr Read wm
type WMOrd wm = WMConstr Ord wm
type WMEq wm = WMConstr Eq wm
```
