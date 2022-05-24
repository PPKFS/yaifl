# The World Model

The `WorldModel` encompasses the spatial object model, as Inform's literate source puts it. This provides us with everything to specify the layout of the game world, the objects within it, and properties they have. This only covers the *foundations* of a world model, rather than specific instantiations. For instance, `Thing`s and their `name` and `description` and `Room`s having `mapConnections`.

## Sections

- [WorldModel and WMx Type Families](worldmodel/typefamilies.md) - A neat trick to allow us to extensively parameterise our universes of types (`Objects`, `Directions`, `Values`, etc) without having to write out a dozen type parameters.
- [The World State](worldmodel/state.md) - The record type that holds the game state.
- [Objects](worldmodel/objects.md) - `Entity`, `Object`, `Store` and how these three types form the backbone of the lookup model.
  - [Entities and Stores](worldmodel/objects/entities-stores.md) - Spoilers: `newtype` wrappers around `Int`s and `newtype` wrappers around `newtype` wrappers around `IntMap` and making it slightly less painful to deal with object lookups.
  - [Objects, Things and Rooms](worldmodel/objects/things.md) - Probably the most important part, actual game objects.
  - [Reification](worldmodel/objects/reification.md) - Dealing with dynamic objects, because doing things as text substitutions is kind of awkward.
  - [ObjectLike](worldmodel/objects/objectlike.md) - Some helpers for things which act a bit like `Object`s, but aren't actually `Object`s (necessarily).
  - [Object Specifics and Object Data](worldmodel/objects/specifics-data.md) - The section that I could never remember the order of, the components that make `Thing`s things (`ObjectData`) and `Supporter`s supporters (`ObjectSpecifics`).
- [Properties](properties.md) - Smaller parts of `ObjectSpecifics` that can be shared by many types of `Object`.
  - [Get, Set, Modify](properties/getsetmodify.md) - `TemplateHaskell` to define our lookup functions. 
