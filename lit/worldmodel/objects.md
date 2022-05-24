# Objects

This chapter contains 3 main parts:

- **Entities** - An ID that allows us to circumvent immutability by breaking apart references between objects.
- **Stores** - `Map`s of `Entity`s to various objects.
- **Objects** - Game objects, spanning from the physical (keys, doors, people) to the intangible (rooms, etc).

Between these 3, we can construct (with some difficulty) a world model that can represent some domain of IF, but not one that can be interacted with -- yet.

### Brief comments on some other systems

There's certainly a couple of other ways to approach this idea (that aren't "just write in OOP"), which I figured were worth mentioning.

- **Entity-component systems (ECS)** - these are certainly doable in Haskell - `ecstasy` and `apecs` come to mind, and to some degree we are designing an ECS that cannot change at runtime - but it's not great because the semantic approach is that you want to iterate over sets of *components* and less so care about what they are attached to -- this is the exact opposite to a text adventure (where you rarely care about *all* doors or vehicles or whatever, rather just a specific one).
- **Ad-hoc, extensible records** - By which I mean some sort of system where there is no distinction between discrete classes of kinds, but rather everything is an anonymous record (plus/minus some type-wizardry) with named fields. This would probably be a good option, but I felt it un-idiomatic. 

### Aims of the Entity/Store/Model structure

- Everything should be Haskell-ish. Whilst the use of `lens` (or in this case, `optics`) is almost a given for a program that works so heavily on nested data structure modification, and it being a "game" implies the existence of some monolithic state, I would like to be able to work in pure functions where possible.  
- Everything should be extensible with minimal effort. Any more direct way to include such semantics as "A thing can be sticky or not sticky. A thing is usually sticky" as modifying the very concept of a `Thing` is getting into dynamic typing territory. Rather, by utilising smart constructors, we can write simple wrappers of `makeStickyThing` that use `ObjectSpecifics` that look like `(Stickiness, a)` and *voila*, we have redefined every `Thing` in the program. NB: this does mean there needs to be some care taken when we automatically generate objects and rooms (the player, the void).
- Type safety. It makes no sense to call `move` on a `Room` and a `Scenery`, for instance.
